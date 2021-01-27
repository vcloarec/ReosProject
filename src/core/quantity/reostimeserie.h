/***************************************************************************
  reostimeserie.h - ReosTimeSerie

 ---------------------
 begin                : 26.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTIMESERIE_H
#define REOSTIMESERIE_H

#include <memory>

#include <QVector>
#include <QDateTime>
#include <QAbstractTableModel>

#include "reosduration.h"
#include "reosparameter.h"

class ReosTimeSerie : public QObject
{
  public:
    ReosTimeSerie( QObject *parent = nullptr );
    virtual ~ReosTimeSerie() = default;

    ReosParameterDateTime *referenceTime() const {return mReferenceTime;}

    int valueCount() const;

    virtual ReosDuration relativeTimeAt( int i ) const = 0 ;
    double valueAt( int i ) const;

  private:
    QVector<double> mValues;
    ReosParameterDateTime *mReferenceTime = nullptr;
};

class ReosTimeSerieConstantInterval: public ReosTimeSerie
{
  public:
    ReosTimeSerieConstantInterval( QObject *parent = nullptr );

    ReosParameterDuration *timeStep() const;

    ReosDuration relativeTimeAt( int i ) const override;

  private:
    ReosParameterDuration *mTimeStep;
};


class ReosTimeSerieModel : public QAbstractTableModel
{
  public:
    ReosTimeSerieModel( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    void setSerieData( std::weak_ptr<ReosTimeSerie> data );
    void setTimeUnit( ReosDuration::Unit unit );
    void setEditable( bool b );

  private:
    std::shared_ptr<ReosTimeSerie> mData;
    ReosDuration::Unit mTimeUnit = ReosDuration::minute;
    bool mIsEditable = false;
};

#endif // REOSTIMESERIE_H
