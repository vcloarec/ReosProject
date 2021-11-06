/***************************************************************************
  reostimeserieprovider.h - ReosTimeSerieProvider

 ---------------------
 begin                : 2.11.2021
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
#ifndef REOSTIMESERIEPROVIDER_H
#define REOSTIMESERIEPROVIDER_H

#include <memory>
#include <QVector>
#include <QDateTime>

#include "reosduration.h"
#include "reosdataobject.h"

/**
 * Abstract class for time serie provider
 */
class ReosTimeSerieProvider : public QObject
{
    Q_OBJECT
  public:
    virtual ~ReosTimeSerieProvider();

    virtual QString key() const = 0;
    virtual bool isEditable() const {return false;}

    virtual QDateTime referenceTime() const = 0;
    virtual void setReferenceTime( const QDateTime &referenceTime );

    virtual QString valueUnit() const = 0;
    virtual int valueCount() const = 0;
    virtual double value( int i ) const = 0;
    virtual double firstValue() const = 0;
    virtual double lastValue() const = 0;

    virtual void setValue( int index, double value );;
    virtual void removeValues( int from, int count );;
    virtual void clear();

    virtual double *data() = 0;
    virtual const QVector<double> &constData() const = 0;

    virtual ReosEncodedElement encode() const = 0;
    virtual void decode( const ReosEncodedElement &element ) = 0;

    virtual void load() {}
    QString dataSource() const;
    void setDataSource( const QString &dataSource );

    virtual QString htmlMetaData() const {return QString();}

  signals:
    void dataChanged();

  private:
    QString mDataSource;
};


/**
 * Abstract class for time serie provider factory
 */
class ReosTimeSerieProviderFactory
{
  public:
    virtual ReosTimeSerieProvider *createProvider() const = 0;
    virtual QString key() const = 0;
};

/**
 * Class that stores time serie provider factory
 */
class ReosTimeSerieProviderRegistery
{
  public:
    ReosTimeSerieProviderRegistery();

    void registerProviderFactory( ReosTimeSerieProviderFactory *factory );

    static ReosTimeSerieProviderRegistery *instance();

    ReosTimeSerieProvider *createProvider( const QString &key );

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosConcentrationTimeFormula> dummy; // work arround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif

    std::map<QString, std::unique_ptr<ReosTimeSerieProviderFactory>> mFactories;
    static ReosTimeSerieProviderRegistery *sInstance;
};

/**
 * Abstract class for constant time step time serie provider factory
 */
class ReosTimeSerieConstantTimeStepProvider : public ReosTimeSerieProvider
{
  public:
    ReosTimeSerieConstantTimeStepProvider() = default;
    ~ReosTimeSerieConstantTimeStepProvider();

    virtual void resize( int size );
    virtual void appendValue( double value );
    virtual void prependValue( double value );
    virtual void insertValue( int pos, double value );

    virtual ReosDuration timeStep() const = 0;
    virtual void setTimeStep( const ReosDuration &timeStep );
};

/**
 * Abstract class for variable time step time serie provider factory
 */
class ReosTimeSerieVariableTimeStepProvider : public ReosTimeSerieProvider
{
  public:
    ReosTimeSerieVariableTimeStepProvider() = default;
    ~ReosTimeSerieVariableTimeStepProvider();

    virtual ReosDuration relativeTimeAt( int i ) const  = 0;
    virtual ReosDuration lastRelativeTime() const = 0;

    virtual void setRelativeTimeAt( int i, const ReosDuration &relativeTime );;
    virtual void appendValue( const ReosDuration &relativeTime, double v );;
    virtual void prependValue( const ReosDuration &relativeTime, double v );;
    virtual void insertValue( int fromPos, const ReosDuration &relativeTime, double v );;
};


//**********************************************************************

class ReosTimeSerieConstantTimeStepMemoryProvider : public ReosTimeSerieConstantTimeStepProvider
{
  public:
    ReosTimeSerieConstantTimeStepMemoryProvider() = default;
    ReosTimeSerieConstantTimeStepMemoryProvider( const QVector<double> &values );

    QString key() const override;
    QDateTime referenceTime() const override;
    void setReferenceTime( const QDateTime &referenceTime );
    ReosDuration timeStep() const override;
    void setTimeStep( const ReosDuration &timeStep );
    QString valueUnit() const override;
    int valueCount() const override;
    void resize( int size ) override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void setValue( int i, double v ) override;
    void appendValue( double v ) override;
    void prependValue( double v ) override;
    void insertValue( int fromPos, double v ) override;
    bool isEditable() const override;
    double *data() override;
    const QVector<double> &constData() const override;
    void removeValues( int fromPos, int count ) override;
    void clear() override;
    ReosEncodedElement encode() const override;
    void decode( const ReosEncodedElement &element ) override;

  private:
    QDateTime mReferenceTime;
    ReosDuration mTimeStep;
    QVector<double> mValues;
};

class ReosTimeSerieConstantTimeStepMemoryProviderFactory : public ReosTimeSerieProviderFactory
{
  public:

    ReosTimeSerieProvider *createProvider() const override {return new ReosTimeSerieConstantTimeStepMemoryProvider;}
    QString key() const override {return QStringLiteral( "constant-time-step-memory" );}
};

//**********************************************************************

class ReosTimeSerieVariableTimeStepMemoryProvider : public ReosTimeSerieVariableTimeStepProvider
{
  public:
    ReosTimeSerieVariableTimeStepMemoryProvider() = default;
    ReosTimeSerieVariableTimeStepMemoryProvider( const QVector<double> &values, const QVector<ReosDuration> &timeValues );

    QString key() const override;

    QDateTime referenceTime() const override;
    void setReferenceTime( const QDateTime &referenceTime ) override;
    QString valueUnit() const override;
    int valueCount() const override;
    double value( int i ) const override;
    double firstValue() const override;
    double lastValue() const override;
    void setValue( int i, double v ) override;
    ReosDuration relativeTimeAt( int i ) const override;
    ReosDuration lastRelativeTime() const override;
    void setRelativeTimeAt( int i, const ReosDuration &relativeTime ) override;
    void appendValue( const ReosDuration &relativeTime, double v ) override;
    void prependValue( const ReosDuration &relativeTime, double v ) override;
    void insertValue( int fromPos, const ReosDuration &relativeTime, double v ) override;
    bool isEditable() const override {return true;}
    double *data() override {return mValues.data();}
    const QVector<double> &constData() const override {return mValues;}
    void removeValues( int fromPos, int count ) override;
    void clear() override;

    ReosEncodedElement encode() const override;
    void decode( const ReosEncodedElement &element ) override;

  private:
    QDateTime mReferenceTime;
    QVector<double> mValues;
    QVector<ReosDuration> mTimeValues;
};


class ReosTimeSerieVariableTimeStepMemoryProviderFactory : public ReosTimeSerieProviderFactory
{
  public:
    ReosTimeSerieProvider *createProvider() const override {return new ReosTimeSerieVariableTimeStepMemoryProvider;}
    QString key() const override {return QStringLiteral( "variable-time-step-memory" );}
};


#endif // REOSTIMESERIEPROVIDER_H
