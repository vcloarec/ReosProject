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
    virtual ~ReosParameter() = default;
    QString name() const;

    virtual QString type() const {return QString();}
    virtual QString toString( int precision = 2 ) const = 0;

    bool isDerivable() const;
    bool isDerived() const;
    bool isValid() const;
    void askForDerivation();

  signals:
    void valueChanged();
    void needDerivation();
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



class ReosParameterDouble: public ReosParameter
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

  private:
    double mValue = 0;
    int mDisplayPrecision = -1;
};

class ReosParameterString: public ReosParameter
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

  private:

    QString mValue;
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

  private:
    double mSlope = 0;
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

  private:
    ReosDuration mDuration;
};

class REOSCORE_EXPORT ReosParameterDateTime: public ReosParameter
{
  public:
    explicit ReosParameterDateTime( const QString &name, bool derivable, QObject *parent = nullptr );
    explicit ReosParameterDateTime( const QString &name, QObject *parent = nullptr );

    void setValue( const QDateTime &dt );
    void setDerivedValue( const QDateTime &dt );

    QDateTime value() {return mDateTime;}

    QString type() const override {return QStringLiteral( "date-time" );}
    QString toString( int = 2 ) const override;

    ReosEncodedElement encode() const;
    static ReosParameterDateTime *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    QDateTime mDateTime;
};


#endif // REOSPARAMETER_H
