/***************************************************************************
                      reosrastertrace.h
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDRASTERTOOLS_H
#define HDRASTERTOOLS_H

#include <QPoint>
#include <QPolygon>
#include <QMutex>

#include "reosmemoryraster.h"

/**
 * A class that create trace between cells in a raster, tracing is done following criteria define in the virtual method defineDirection()
 */
class ReosRasterTraceBetweenCells
{
  public:
    //! Starts the tracing
    bool startTracing();

    //! Returns the actuel trace
    QPolygon trace();

    //! Returns error code
    int error() const;

    //! Stops the tracing
    void stopTrace();

    //! Returns whether the trace was stopped
    bool isStopped();

  protected:

    ReosRasterTraceBetweenCells( const QPoint &start,
                                 const QPoint &origin,
                                 const QVector<QPoint> &stopLine,
                                 QList<QPoint> &elimination );

    virtual ~ReosRasterTraceBetweenCells();

    void savePosition( bool saveBetweePosition );
    void move( bool forceSaving );
    void move();
    QPoint position() const {return mPosition;}
    QPoint direction() const {return mDirection;}
    int testPosition( const QPoint &p, const QPoint &dir, int cellCount );

    //! Return the index of p in the stop line if dir is the direction of the line, return - if not or if p is not in the stop line
    virtual int isInStopLine( const QPoint &p, const QPoint &dir ); /*<!return the index of the pixel of ligneArret_*/

    virtual void setToBeEliminated( const QPoint &p );
    virtual bool hasToBeEliminated( const QPoint &p ) const;
    void verticalSegments( QList<QPoint> &sv ) {sv = mVerticalSegments;}
    void horizontalSegments( QList<QPoint> &sv ) {sv = mHorizontalSegments;}

    void addVerticalSegments( QList<QPoint> &sv ) {sv << mVerticalSegments;}
    void addHorizontalSegments( QList<QPoint> &sh ) {sh << mHorizontalSegments;}


    virtual void defineDirection( QList<QPoint> &listDir ) const = 0;
    virtual void eliminateDirection( QList<QPoint> &listDir );

    virtual bool startNewTrace( const QPoint &dep,
                                const QPoint &orig,
                                QList<QPoint> &elim,
                                int &erreur,
                                QPolygon &newTrace,
                                QPoint &resPos,
                                QPoint &resDir,
                                QList<QPoint> &sv,
                                QList<QPoint> &sh ) = 0;

    virtual ReosRasterTraceBetweenCells *newTrace( const QPoint &dep, const QPoint &Orig, QList<QPoint> &elim ) = 0;

    virtual int arrivalTest( const QPoint &p, const QPoint &dir );

    // Calculate the two adjacent cells from a position between cells and direction
    static QVector<QPoint> fromBetweenAndDirectionToCells( const QPoint &interpixel, const QPoint &dir );

    virtual bool isValid() const = 0;


    //Tracing constraints
    QPoint mStart;
    QVector<QPoint> mStopLine;
    QList<QPoint> &mCellsToEliminate;
    int mLimit = 20000000;


    //Tracing attribute
    QPoint mPosition;
    QPointF mPositionF;
    QPoint mOrigin;
    QPoint mDirection;

    QPolygon mTrace;
    int mTreatedCells;
    QList<QPoint> mVerticalSegments;
    QList<QPoint> mHorizontalSegments;

    const QVector<QPoint> mCellStart;

    int mErrorCode;

    bool mIsStopped;

    QVector<ReosRasterTraceBetweenCells *> mThrownTrace;
};

/**
 * A class that create trace between cells in a raster, tracing is done following a unique value in the raster
 */
template<typename T>
class ReosRasterTraceBetweenCellsUniqueValue: public ReosRasterTraceBetweenCells
{
  public:
    /**
     * Constructor
     *
     * \param raster the raster concerned
     * @param value the value to consider
     * @param start the starting poinr
     * @param origin the origine where come from the tracing
     * @param stopLine a line where the tracing will stop when the tracing will meet
     * @param elimination a ist of point to not consider during tracing, could be void and will be filled when tracing
     */
    ReosRasterTraceBetweenCellsUniqueValue(
      ReosRasterMemory<T> raster,
      T value,
      const QPoint &start,
      const QPoint &origin,
      const QVector<QPoint> &stopLine,
      QList<QPoint> &elimination );

  protected:
    virtual void defineDirection( QList<QPoint> &listDir ) const;
    virtual bool startNewTrace( const QPoint &dep,
                                const QPoint &orig,
                                QList<QPoint> &elim,
                                int &erreur,
                                QPolygon &newTrace,
                                QPoint &resPos,
                                QPoint &resDir,
                                QList<QPoint> &sv, QList<QPoint> &sh ) override;

    virtual ReosRasterTraceBetweenCells *newTrace( const QPoint &start, const QPoint &ori, QList<QPoint> &elim ) override;

    bool isValid() const override;

  private:
    int mValue;
    ReosRasterMemory<T> mRaster;
};

template<typename T>
ReosRasterTraceBetweenCellsUniqueValue<T>::ReosRasterTraceBetweenCellsUniqueValue( ReosRasterMemory<T> raster, T value, const QPoint &start, const QPoint &origin, const QVector<QPoint> &stopLine, QList<QPoint> &elimination ):
  ReosRasterTraceBetweenCells( start, origin, stopLine, elimination ),
  mValue( value ),
  mRaster( raster )
{}

template<typename T>
void ReosRasterTraceBetweenCellsUniqueValue<T>::defineDirection( QList<QPoint> &listDir ) const
{
  int i = 0;
  while ( i < listDir.count() )
  {
    QVector<QPoint> cellToTest = fromBetweenAndDirectionToCells( mPosition, listDir[i] );

    T v1 = mRaster.value( cellToTest[0].y(), cellToTest[0].x() );
    T v2 = mRaster.value( cellToTest[1].y(), cellToTest[1].x() );

    bool condition = ( v1 != v2 ) && ( ( v1 == mValue ) || ( v2 == mValue ) );

    if ( condition )
      ++i;
    else
      listDir.removeAt( i );
  }
}

template<typename T>
bool ReosRasterTraceBetweenCellsUniqueValue<T>::startNewTrace(
  const QPoint &dep,
  const QPoint &orig,
  QList<QPoint> &elim,
  int &erreur,
  QPolygon &newTrace,
  QPoint &resPos,
  QPoint &resDir,
  QList<QPoint> &sv,
  QList<QPoint> &sh )
{
  ReosRasterTraceBetweenCellsUniqueValue otherTrace( mRaster, mValue, dep, orig, mStopLine, elim );
  int result = otherTrace.startTracing();
  erreur = otherTrace.error();
  newTrace = otherTrace.trace();
  resPos = otherTrace.position();
  resDir = otherTrace.direction();
  otherTrace.verticalSegments( sv );
  otherTrace.horizontalSegments( sh );
  return result;
}

template<typename T>
ReosRasterTraceBetweenCells *ReosRasterTraceBetweenCellsUniqueValue<T>::newTrace( const QPoint &start, const QPoint &ori, QList<QPoint> &elim )
{
  return new ReosRasterTraceBetweenCellsUniqueValue( mRaster, mValue, start, ori, mStopLine, elim );
}

template<typename T>
bool ReosRasterTraceBetweenCellsUniqueValue<T>::isValid() const
{
  return ( mRaster.isValid() );
}

#endif // HDRASTERTOOLS_H
