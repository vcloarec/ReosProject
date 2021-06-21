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
#include "reosdataobject.h"


//! Class that handle time serie data
class REOSCORE_EXPORT ReosTimeSerie : public ReosDataObject
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
    virtual void setValueAt( int i, double value );

    //! Removes \a count values from position \a fromPos
    void removeValues( int fromPos, int count );

    //! Clears all values
    void clear();

    //! Return the value extent of the serie, if withZero, zeo will be a extrem if all values are positive or negative
    QPair<double, double> valueExent( bool withZero = false ) const;

    //! Encodes/Decodes base information in the \a element
    virtual void baseEncode( ReosEncodedElement &element ) const;
    virtual bool  decodeBase( const ReosEncodedElement &element );

    //! Returns the text unit of values
    QString valueUnit() const;

    //! Sets the text unit of values
    void setValueUnit( const QString &valueUnit );

    //! Returns a pointer to access directly to the data, can be used only  when need efficient calculation.
    double *data() {return mValues.data();}

    const QVector<double> &constData() const {return  mValues;}

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
class REOSCORE_EXPORT ReosTimeSerieConstantInterval: public ReosTimeSerie
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
    virtual void setValueAt( int i, double value ) override;
    QString type() const override {return QStringLiteral( "time-serie-constant-interval" );}

    //! Overrides the type
    void setType( const QString &dataType );

    void appendValue( double value );
    void insertValues( int fromPos, int count, double value );

    //! Returns the current value mode
    ValueMode valueMode() const;

    //! Set the value mode to \a valueMode
    void setValueMode( const ValueMode &valueMode );

    //! Returns a pointer to the constant time step parameter
    ReosParameterDuration *timeStep() const;

    ReosDuration::Unit intensityTimeUnit() const;
    void setIntensityTimeUnit( const ReosDuration::Unit &intensityTimeUnit );

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

    //! Return current mode value unit string
    QString unitStringCurrentMode() const;

    //! Returns the color attributed to the current mode
    QColor currentValueModeColor() const;

    void setValueModeColor( ValueMode mode, const QColor &color );

    //! Returns a flag to make this instance using cumulative mode in addition to the current mode
    bool addCumultive() const;
    //! Sets a flag to make this instance using cumulative mode in addition to the current mode
    void setAddCumulative( bool addCumulative );

    //! Connects attribute with \a other
    void syncWith( ReosTimeSerieConstantInterval *other );

    //! Copy atributes from \a other to \a this
    void copyAttribute( ReosTimeSerieConstantInterval *other );

    //! Returns a encoded element correspondint to this serie
    ReosEncodedElement encode( const QString &descritpion = QString() ) const;

    //! Creates new instance from the encoded element
    static ReosTimeSerieConstantInterval *decode( const ReosEncodedElement &element, QObject *parent = nullptr );


  protected:
    void connectParameters();

    ReosTimeSerieConstantInterval( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mTimeStep = nullptr;

    //! Attribute that are defined during runtime
    ValueMode mValueMode = Value;
    ReosDuration::Unit mIntensityTimeUnit = ReosDuration::hour;
    QMap<ValueMode, QString> mValueModeName;
    QMap<ValueMode, QColor> mValueModeColor;
    bool mAddCumulative = false;

    double convertFromIntensityValue( double v );
};


//! Model used to handle time series with constant time step
class REOSCORE_EXPORT ReosTimeSerieConstantIntervalModel : public QAbstractTableModel
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

class REOSCORE_EXPORT ReosTimeSerieVariableTimeStep: public ReosTimeSerie
{
    Q_OBJECT
  public:

    ReosTimeSerieVariableTimeStep( QObject *parent );

    QString type() const {return QStringLiteral( "time-serie-variable-time-step" );}

    ReosDuration relativeTimeAt( int i ) const;
    QPair<QDateTime, QDateTime> timeExtent() const;

    //! Sets the value at \a relative time with \a value, if the \a relative time is not present insert a new couple (time, value)
    void setValue( const ReosDuration &relativeTime, double value );

    //! Returns the value at relative time \a relative time, interpolate if relative time is between two time values, return 0 if before first one or after last one
    double valueAtTime( const ReosDuration &relativeTime ) const;

    //! Adds another instance to this the values of this ones, create new time step if needed
    void addOther( const ReosTimeSerieVariableTimeStep &other, double factor = 1 );

    //! Return the color associated with this time serie
    virtual QColor color() const {return QColor();}

  private:
    QVector<ReosDuration> mTimeValues;

    /**
     *  Returns the index of the time value if the value is present, or the index of the value just before if not present (-1 if less than the first one)
     *  If \a time exactly corresponds to an existing index, return true in \a exact
     */
    int timeValueIndex( const ReosDuration &time, bool &exact ) const;

};

#endif // REOSTIMESERIE_H
