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

#include <QColor>
#include <QVector>
#include <QDateTime>
#include <QAbstractTableModel>

#include "reosduration.h"
#include "reosparameter.h"


class ReosDataObject: public QObject
{
    Q_OBJECT
  public:
    ReosDataObject( QObject *parent = nullptr ): QObject( parent ) {}
    virtual QString type() const = 0;

    //! Returns the name of the data object
    QString name() const;

  public slots:
    //! Sets the name of the data object
    void setName( const QString &name );

  signals:
    void dataChanged();
    void settingsChanged();

  private:
    QString mName;
};
//! Class that handle time serie data
class ReosTimeSerie : public ReosDataObject
{
  public:
    ReosTimeSerie( QObject *parent = nullptr );

    //! Returns a pointer to the refrence time parameter
    ReosParameterDateTime *referenceTime() const {return mReferenceTime;}

    //! Returns the count of value in the time serie
    int valueCount() const;

    //! Returns the relative time frm the reference time for the value at position \a i
    virtual ReosDuration relativeTimeAt( int i ) const = 0 ;

    //! returns the absolute time for the value at position \a i
    virtual QDateTime timeAt( int i ) const;

    //! Returns the time extent of the serie
    virtual QPair<QDateTime, QDateTime> timeExtent() const = 0;

    //! Returns the value at positon \a i
    virtual double valueAt( int i ) const;

    //! Sets the \a value at position \a i
    void setValueAt( int i, double value );

    //! Apppends the \a value
    void appendValue( double value );

    //! Removes \a count values from position \a fromPos
    void removeValues( int fromPos, int count );

    //! Inserts \a count values from position \a fromPos with \a value
    void insertValues( int fromPos, int count, double value );

    //! Return the value extent of the serie
    QPair<double, double> valueExent() const;

    //! Encodes/Decodes base information in the \a element
    virtual void baseEncode( ReosEncodedElement &element ) const;
    virtual bool  decodeBase( const ReosEncodedElement &element );

    //! Returns the text unit of values
    QString valueUnit() const;

    //! Sets the text unit of values
    void setValueUnit( const QString &valueUnit );

  protected:
    //! Connect all parameters with
    void connectParameters();
    QVector<double> mValues;

  private:
    ReosParameterDateTime *mReferenceTime = nullptr;
    QString mValueUnit;
};

/**
 *  Class that handle time serie data with constant time interval
 *
 *  By default, value are considered as incremental value. Different modes (\see ValueMode) can be used to return
 *   intensity value ( incremental value divided by the time step) or cumulative value (sum from the begining).
 */
class ReosTimeSerieConstantInterval: public ReosTimeSerie
{
    Q_OBJECT
  public:
    enum ValueMode
    {
      Value,
      Intensity,
      Cumulative
    };

    ReosTimeSerieConstantInterval( QObject *parent = nullptr );

    ReosDuration relativeTimeAt( int i ) const override;
    QPair<QDateTime, QDateTime> timeExtent() const override;
    double valueAt( int i ) const override;
    QString type() const override;

    //! Returns the current value mode
    ValueMode valueMode() const;

    //! Set the value mode to \a valueMode
    void setValueMode( const ValueMode &valueMode );

    //! Returns a pointer to the constant time step parameter
    ReosParameterDuration *timeStep() const;

    //! Returns value at position \a i considering the \a mode
    double valueWithMode( int i, ValueMode mode = Value ) const;

    //! Returns the value extent considering the \a mode
    QPair<double, double> extentValueWithMode( ValueMode mode = Value ) const;

    //! Returns the name of the data considering the \a mode
    QString valueModeName( ValueMode mode ) const;

    //! Sets the \a name of the data considering the \a mode
    void setValueModeName( ValueMode mode, const QString &name );

    //! Returns the color attributed to the data cnsidering the \a mode
    QColor valueModeColor( ValueMode mode ) const;

    //! Returns the color attributed to the current mode
    QColor currentValueModeColor() const;

    void setValueModeColor( ValueMode mode, const QColor &color );

    //! Returns a encoded element correspondint to this serie
    ReosEncodedElement encode() const;

    //! Returns a flag to make this instance using cumulative mode in addition to the current mode
    bool addCumultive() const;
    //! Sets a flag to make this instance using cumulative mode in addition to the current mode
    void setAddCumultive( bool addCumultive );

    //! Creates new instance from the encoded element
    static ReosTimeSerieConstantInterval *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  protected:
    void connectParameters();

  private:
    ReosParameterDuration *mTimeStep = nullptr;
    ValueMode mValueMode = Value;
    QMap<ValueMode, QString> mValueModeName;
    QMap<ValueMode, QColor> mValueModeColor;
    bool mAddCumultive = false;
};


//! Model used to handle time series with constant time step
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

    void setValues( const QModelIndex &fromIndex, const QList<double> &values );
    void deleteValueRows( const QModelIndex &fromIndex, int count );
    void insertValueRows( const QModelIndex &fromIndex, int count );

  private:
    ReosTimeSerieConstantInterval *mData;
    bool mIsEditable = true;
    double defaultValue = 0;

};

#endif // REOSTIMESERIE_H
