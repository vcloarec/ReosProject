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


class ReosDataObject: public QObject
{
  public:
    ReosDataObject( QObject *parent = nullptr ): QObject( parent ) {}

    virtual QString type() const = 0;
};

class ReosTimeSerie : public ReosDataObject
{
  public:
    ReosTimeSerie( QObject *parent = nullptr );
    ReosParameterDateTime *referenceTime() const {return mReferenceTime;}

    int valueCount() const;

    virtual ReosDuration relativeTimeAt( int i ) const = 0 ;
    double valueAt( int i ) const;

    //! Encodes base information in the \a element
    virtual void baseEncode( ReosEncodedElement &element ) const;
    virtual bool  decodeBase( const ReosEncodedElement &element );

  protected:
    QVector<double> mValues;

  private:
    ReosParameterDateTime *mReferenceTime = nullptr;
};

class ReosTimeSerieConstantInterval: public ReosTimeSerie
{
    Q_OBJECT
  public:
    ReosTimeSerieConstantInterval( QObject *parent = nullptr );
    ReosParameterDuration *timeStep() const;

    ReosDuration relativeTimeAt( int i ) const override;

    void setValueAt( int i, double value );
    void appendValue( double value );
    double valueAt( int i ) const;
    QString type() const override;

    ReosEncodedElement encode() const;

    static ReosTimeSerieConstantInterval *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mTimeStep = nullptr;
};


class ReosTimeSerieConstantIntervalModel : public QAbstractTableModel
{
  public:
    ReosTimeSerieConstantIntervalModel( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    Qt::ItemFlags flags( const QModelIndex &index ) const override;

    void setSerieData( ReosTimeSerieConstantInterval *data );
    void setEditable( bool b );

  private:
    ReosTimeSerieConstantInterval *mData;
    bool mIsEditable = true;

};

#endif // REOSTIMESERIE_H
