/***************************************************************************
  reosparameter.h - ReosParameter

 ---------------------
 begin                : 22.1.2021
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
#ifndef REOSPARAMETER_H
#define REOSPARAMETER_H

#include <QObject>
#include <QDateTime>

#include "reoscore.h"
#include "reosarea.h"
#include "reosduration.h"


class REOSCORE_EXPORT ReosParameter : public QObject
{
    Q_OBJECT
  public:
    explicit ReosParameter( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameter( const QString &name, QObject *parent = nullptr );
    virtual ~ReosParameter() = default;
    QString name() const;
    void setName( const QString &name );

    virtual QString type() const {return QString();}
    virtual QString toString( int precision = 2 ) const = 0;

    bool isDerivable() const;
    bool isDerived() const;
    bool isValid() const;
    void askForDerivation();
    void setInvalid();

  public slots:
    void updateIfNecessary();

  signals:
    void valueChanged();
    void needCalculation();
    void unitChanged();

  protected:
    bool mIsDerived = false;
    bool mIsValid = false;
    void setDerivable( bool b );
    void encode( ReosEncodedElement &element ) const;
    void decode( const ReosEncodedElement &element, bool isDerivable );

  private:
    QString mName;
    bool mIsDerivable = false;
};

class REOSCORE_EXPORT ReosParameterDouble: public ReosParameter
{
  public:
    explicit ReosParameterDouble( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterDouble( const QString &name, QObject *parent = nullptr );

    QString type() const override {return QString( "double" );}

    void setValue( double value );
    bool setValueWithString( const QString &value );
    void setDerivedValue( double value );
    double value() const {return mValue;}
    QString toString( int precision = -1 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterDouble *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterDouble *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    double mValue = 0;
    int mDisplayPrecision = -1;
};

class REOSCORE_EXPORT ReosParameterInteger: public ReosParameter
{
  public:
    explicit ReosParameterInteger( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterInteger( const QString &name, QObject *parent = nullptr );

    QString type() const override {return QString( "integer" );}

    void setValue( int value );
    bool setValueWithString( const QString &value );
    void setDerivedValue( int value );
    int value() const {return mValue;}
    QString toString( int = 0 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterInteger *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterInteger *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    int mValue = 0;
};

class REOSCORE_EXPORT ReosParameterString: public ReosParameter
{
  public:
    explicit ReosParameterString( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterString( const QString &name, QObject *parent = nullptr );

    void setValue( const QString &string );
    QString value() const {return mValue;}
    QString type() const override {return QStringLiteral( "string" );}
    QString toString( int = 2 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterString *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterString *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  protected:
    QString mValue;
};

class REOSCORE_EXPORT ReosParameterLongString: public ReosParameterString
{
  public:
    explicit ReosParameterLongString( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterLongString( const QString &name, QObject *parent = nullptr );

    QString type() const override {return QStringLiteral( "long-string" );}

    ReosEncodedElement encode() const;
    static ReosParameterLongString *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterLongString *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

};

class REOSCORE_EXPORT ReosParameterArea: public ReosParameter
{
  public:
    explicit ReosParameterArea( const QString &name, bool derivable,  QObject *parent = nullptr );
    explicit ReosParameterArea( const QString &name,  QObject *parent = nullptr );

    void setValue( const ReosArea &area );
    void setDerivedValue( const ReosArea &area );
    void changeUnit( ReosArea::Unit unit );
    ReosArea value() const {return mValue;}

    QString type() const override {return QStringLiteral( "area" );}
    QString toString( int precision = 2 ) const  override;

    ReosEncodedElement encode() const;
    static ReosParameterArea *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterArea *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    ReosArea mValue;
};

class REOSCORE_EXPORT ReosParameterSlope: public ReosParameter
{
  public:
    explicit ReosParameterSlope( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterSlope( const QString &name, QObject *parent = nullptr );

    void setValue( double slope );
    void setDerivedValue( double slope );

    double value() const {return mSlope;}

    QString type() const override {return QStringLiteral( "slope" );}
    QString toString( int precision = 2 ) const override;


    ReosEncodedElement encode() const;
    static ReosParameterSlope *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterSlope *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    double mSlope = std::numeric_limits<double>::quiet_NaN();
};

class REOSCORE_EXPORT ReosParameterDuration: public ReosParameter
{
  public:
    explicit ReosParameterDuration( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterDuration( const QString &name, QObject *parent = nullptr );

    void setValue( const ReosDuration &duration );
    void setDerivedValue( const ReosDuration &duration );
    void changeUnit( ReosDuration::Unit unit );
    ReosDuration value() const;

    QString type() const override {return QStringLiteral( "duration" );}
    QString toString( int precision = 2 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterDuration *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterDuration *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    ReosDuration mDuration;
};

class REOSCORE_EXPORT ReosParameterDateTime: public ReosParameter
{
  public:
    //explicit ReosParameterDateTime( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterDateTime( const QString &name, QObject *parent = nullptr );

    void setValue( const QDateTime &dt );
    void setDerivedValue( const QDateTime &dt );

    QDateTime value() {return mDateTime;}

    QString type() const override {return QStringLiteral( "date-time" );}
    QString toString( int = 2 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterDateTime *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterDateTime *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    QDateTime mDateTime;
};

class REOSCORE_EXPORT ReosParameterBoolean : public ReosParameter
{
  public:
    explicit ReosParameterBoolean( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterBoolean( const QString &name, QObject *parent = nullptr );

    QString type() const override {return QString( "boolean" );}

    void setValue( bool value );
    void setDerivedValue( bool value );
    bool value() const {return mValue;}
    QString toString( int = -1 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterBoolean *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );
    static ReosParameterBoolean *decode( const ReosEncodedElement &element, bool isDerivable, const QString &name, QObject *parent );

  private:
    bool mValue = 0;
};


#endif // REOSPARAMETER_H
