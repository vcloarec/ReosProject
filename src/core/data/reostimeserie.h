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
#include <QPointer>
#include <QVector>
#include <QDateTime>
#include <QAbstractTableModel>

#include "reosduration.h"
#include "reosparameter.h"
#include "reosdataobject.h"
#include "reostimeserieprovider.h"

//! Class that handle time serie data
class REOSCORE_EXPORT ReosTimeSerie : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosTimeSerie( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() );

    QString type() const override {return staticType();}

    //! Returns a pointer to the refrence time parameter
    ReosParameterDateTime *referenceTimeParameter() const {return mReferenceTimeParameter;}

    //! Sets the reference time
    void setReferenceTime( const QDateTime &dateTime );

    //! Returns the reference time
    QDateTime referenceTime() const;

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
    virtual  void clear();

    //! Return the value extent of the serie, if withZero, zeo will be a extrem if all values are positive or negative
    QPair<double, double> valueExent( bool withZero = false ) const;

    //! Returns the text unit of values
    QString valueUnit() const;

    //! Sets the text unit of values
    void setValueUnit( const QString &valueUnit );

    //! Returns a pointer to access directly to the data, can be used only  when need efficient calculation.
    double *data();

    //! Returns the maximum of values of this time serie
    double maximum() const;

    //! Returns the maximum of values of this time serie
    double minimum() const;

    const QVector<double> &constData() const;

    virtual ReosTimeSerieProvider *dataProvider() const;

    static QString staticType() {return ReosDataObject::staticType() + ':' + QStringLiteral( "time-serie" );}

  protected slots:
    virtual void onDataProviderChanged();

  protected:
    //! Connect all parameters with
    void connectParameters();

    //! Encodes/Decodes base information in the \a element
    virtual void baseEncode( ReosEncodedElement &element ) const;
    virtual bool decodeBase( const ReosEncodedElement &element );

    std::unique_ptr<ReosTimeSerieProvider> mProvider;

  private:
    ReosParameterDateTime *mReferenceTimeParameter = nullptr;
    QString mValueUnit;

    double mMaximum = -std::numeric_limits<double>::max();
    double mMinimum = std::numeric_limits<double>::max();

    void updateStats();
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

    ReosTimeSerieConstantInterval( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() );

    ReosDuration relativeTimeAt( int i ) const override;
    QPair<QDateTime, QDateTime> timeExtent() const override;
    double valueAt( int i ) const override;
    void setValueAt( int i, double value ) override;

    QString type() const override {return staticType();}
    static QString staticType() {return ReosTimeSerie::staticType() + ':' + QStringLiteral( "constant-interval" );}

    //! Overrides the type
    void setType( const QString &dataType );

    void appendValue( double value );
    void insertValues( int fromPos, int count, double value );

    //! Returns the current value mode
    ValueMode valueMode() const;

    //! Set the value mode to \a valueMode
    void setValueMode( const ValueMode &valueMode );

    //! Returns a pointer to the constant time step parameter
    ReosParameterDuration *timeStepParameter() const;

    //! Sets the constant time step of this time serie
    void setTimeStep( const ReosDuration &timeStep );

    //! Returns the constant time step of this time serie
    ReosDuration timeStep() const;

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

    //! Returns a encoded element corresponding to this serie
    ReosEncodedElement encode( const QString &descritpion = QString() ) const;

    //! Creates new instance from the encoded element
    static ReosTimeSerieConstantInterval *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

    ReosTimeSerieConstantTimeStepProvider *constantTimeStepDataProvider() const;

    //! Copy the serie \a other in \a this.
    void copyFrom( ReosTimeSerieConstantInterval *other );

  private slots:
    void onDataProviderChanged() override;

  protected:
    void connectParameters();
    ReosTimeSerieConstantInterval( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    ReosParameterDuration *mTimeStepParameter = nullptr;

    //! Attribute that are defined during runtime
    ValueMode mValueMode = Value;
    ReosDuration::Unit mIntensityTimeUnit = ReosDuration::hour;
    QMap<ValueMode, QString> mValueModeName;
    QMap<ValueMode, QColor> mValueModeColor;
    bool mAddCumulative = false;

    double convertFromIntensityValue( double v );
};

class ReosTimeSerieVariableTimeStepModel;

class REOSCORE_EXPORT ReosTimeSerieVariableTimeStep: public ReosTimeSerie
{
    Q_OBJECT
  public:
    ReosTimeSerieVariableTimeStep( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosTimeSerie::staticType() + ':' + QStringLiteral( "variable-time-step" );}

    //! Returns the relative time at \a i
    ReosDuration relativeTimeAt( int i ) const override;

    //! Sets the relative time \a relativeTime at postion \a i. If the value is not compatible with previous or next values, do nothing and return false.
    bool setRelativeTimeAt( int i, const ReosDuration &relativeTime );

    //! Sets the relative time \a relativeTime at postion \a i regardless of the value of \a relativeTime and of other time values
    void setAnyRelativeTimeAt( int i, const ReosDuration &relativeTime );

    QPair<QDateTime, QDateTime> timeExtent() const override;

    //! Sets the value at \a relative time with \a value, if the \a relative time is not present insert a new couple (time, value)
    void setValue( const ReosDuration &relativeTime, double value );

    //! Sets the value at \a time with \a value, if the \a relative time is not present insert a new couple (time, value)
    void setValue( const QDateTime &time, double value );

    //! Returns the value at relative time \a relative time, interpolate if relative time is between two time values, return 0 if before first one or after last one
    double valueAtTime( const ReosDuration &relativeTime ) const;

    //! Returns the value at time \a time, interpolate if time is between two time values, return 0 if before first one or after last one
    double valueAtTime( const QDateTime &time ) const;

    //! Adds another instance to this the values of this ones, create new time steps if needed
    void addOther( const ReosTimeSerieVariableTimeStep *other, double factor = 1, bool allowInterpolation = true );

    //! Returns the unit of the values as a string
    QString unitString() const;

    //! Sets the unit of the values as a string
    void setUnitString( const QString &unitString );

    //! Returns the color used to render the time serie
    QColor color() const;

    //! Sets the color used to render the time serie
    void setColor( const QColor &color );

    void copyFrom( ReosTimeSerieVariableTimeStep *other );

    bool operator==( ReosTimeSerieVariableTimeStep &other ) const;

    QAbstractItemModel *model();

    ReosEncodedElement encode() const;
    static ReosTimeSerieVariableTimeStep *decode( const ReosEncodedElement &element, QObject *parent = nullptr );

  public slots:
    //! Sets indirectly the color from an object that handle common color for time series
    void setCommonColor( const QColor &color );

  signals:
    void colorChanged( const QColor &color );
    void displayColorChanged( const QColor &color );

  protected:

    //! Encodes/Decodes base information in/from the \a element
    virtual void baseEncode( ReosEncodedElement &element ) const override;
    virtual bool  decodeBase( const ReosEncodedElement &element ) override;

    ReosTimeSerieVariableTimeStepProvider *variableTimeStepdataProvider() const;

  private:
    QString mUnitString;
    QColor mColor;

    ReosTimeSerieVariableTimeStepModel *mModel = nullptr;


    /**
     *  Returns the index of the time value if the value is present, or the index of the value just before if not present (-1 if less than the first one)
     *  If \a time exactly corresponds to an existing index, return true in \a exact
     */
    int timeValueIndex( const ReosDuration &time, bool &exact ) const;
};

class REOSCORE_EXPORT ReosTimeSerieModel : public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosTimeSerieModel( QObject *parent = nullptr )
    {}

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;

    virtual void setValues( const QModelIndex &fromIndex, const QList<QVariantList> &values ) = 0;
    virtual void insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values ) = 0;

    QList<int> editableColumn() const;

  public slots:
    virtual void deleteRows( const QModelIndex &fromIndex, int count ) = 0;
    virtual void insertRows( const QModelIndex &fromIndex, int count ) = 0;

};

//! Model used to handle AND edit time series with constant time step
class REOSCORE_EXPORT ReosTimeSerieConstantIntervalModel : public ReosTimeSerieModel
{
    Q_OBJECT
  public:
    ReosTimeSerieConstantIntervalModel( QObject *parent = nullptr );

    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;

    void setSerieData( ReosTimeSerieConstantInterval *data );

    void setValues( const QModelIndex &fromIndex, const QList<QVariantList> &values ) override;
    void insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values ) override;
    void deleteRows( const QModelIndex &fromIndex, int count ) override;
    void insertRows( const QModelIndex &fromIndex, int count ) override;

    bool isEditable() const;

  private:
    ReosTimeSerieConstantInterval *mData;
    double mDefaultValue = 0;
    void setValues( const QModelIndex &fromIndex, const QList<double> &values );

    static QList<double> doubleFromVariantList( const QList<QVariantList> &values );
};

//! Model used to handle AND edit time series with variable time step
class REOSCORE_EXPORT ReosTimeSerieVariableTimeStepModel: public ReosTimeSerieModel
{
    Q_OBJECT
  public:
    ReosTimeSerieVariableTimeStepModel( QObject *parent = nullptr );

    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    void setValues( const QModelIndex &fromIndex, const QList<QVariantList> &data ) override;
    void insertValues( const QModelIndex &fromIndex, const QList<QVariantList> &values ) override;
    void deleteRows( const QModelIndex &fromIndex, int count ) override;
    void insertRows( const QModelIndex &fromIndex, int count ) override;

    void setSerie( ReosTimeSerieVariableTimeStep *serie );
    void setNewRowWithFixedTimeStep( bool newRowWithFixedTimeStep );
    void setFixedTimeStep( const ReosDuration &fixedTimeStep );
    void setVariableTimeStepUnit( const ReosDuration::Unit &variableTimeStepUnit );

    bool isEditable() const;

  private slots:
    void updateModel();

  private:
    QPointer<ReosTimeSerieVariableTimeStep> mData;
    bool mIsEditable = true;
    double mDefaultValue = 0.0;
    bool mNewRowWithFixedTimeStep = true;
    ReosDuration mFixedTimeStep;
    ReosDuration::Unit mVariableTimeStepUnit = ReosDuration::minute;

    int valueColumn() const;

    bool checkListValuesValidity( const QModelIndex &index, const QList<QVariantList> &data, bool insert );
    void setValuesPrivate( const QModelIndex &fromIndex, const QList<QVariantList> &data, bool checkValidity );
    bool insertRowsPrivate( const QModelIndex &fromIndex, int count, bool followdBySetValueWithTime );
};

#endif // REOSTIMESERIE_H
