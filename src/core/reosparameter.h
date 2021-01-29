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
    explicit ReosParameter( const QString &name, QObject *parent = nullptr );
    virtual ~ReosParameter() = default;
    QString name() const;

    virtual QString type() {return QString();}

    bool isDerivable() const;
    bool isDerived() const;
    void askForDerivation();

  signals:
    void valueChanged();
    void needDerivation();

  protected:
    bool mIsDerived;
    void setDerivable( bool b );
    void encode( ReosEncodedElement &element ) const;
    void decode( const ReosEncodedElement &element, bool isDerivable );

  private:
    QString mName;
    bool mIsDerivable = false;

};

class ReosParameterString: public ReosParameter
{
  public:
    explicit ReosParameterString( const QString &name, QObject *parent = nullptr );

    void setValue( const QString &string );
    QString value() const {return mValue;}
    QString type() {return QStringLiteral( "string" );}

    ReosEncodedElement encode() const;
    static ReosParameterString *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:

    QString mValue;
};

class REOSCORE_EXPORT ReosParameterArea: public ReosParameter
{
  public:
    explicit ReosParameterArea( const QString &name, QObject *parent = nullptr );

    void setValue( const ReosArea &area );
    void setDerivedValue( const ReosArea &area );
    void changeUnit( ReosArea::Unit unit );
    ReosArea value() const {return mValue;}

    QString type() {return QStringLiteral( "area" );}

    ReosEncodedElement encode() const;
    static ReosParameterArea *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    ReosArea mValue;
};

class REOSCORE_EXPORT ReosParameterSlope: public ReosParameter
{
  public:
    explicit ReosParameterSlope( const QString &name, QObject *parent = nullptr );

    void setValue( double slope );
    void setDerivedValue( double slope );

    double value() {return mSlope;}

    QString type() override {return QStringLiteral( "slope" );}

    ReosEncodedElement encode() const;
    static ReosParameterSlope *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    double mSlope = 0;
};

class REOSCORE_EXPORT ReosParameterDuration: public ReosParameter
{
  public:
    explicit ReosParameterDuration( const QString &name, QObject *parent = nullptr );

    void setValue( const ReosDuration &duration );
    void setDerivedValue( const ReosDuration &duration );
    void changeUnit( ReosDuration::Unit unit );
    ReosDuration value() const;

    QString type() override {return QStringLiteral( "duration" );}

    ReosEncodedElement encode() const;
    static ReosParameterDuration *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    ReosDuration mDuration;
};

class REOSCORE_EXPORT ReosParameterDateTime: public ReosParameter
{
  public:
    explicit ReosParameterDateTime( const QString &name, QObject *parent = nullptr );

    void setValue( const QDateTime &dt );
    void setDerivedValue( const QDateTime &dt );

    QDateTime value() {return mDateTime;}

    QString type() override {return QStringLiteral( "date-time" );}

    ReosEncodedElement encode() const;
    static ReosParameterDateTime *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    QDateTime mDateTime;
};


#endif // REOSPARAMETER_H
