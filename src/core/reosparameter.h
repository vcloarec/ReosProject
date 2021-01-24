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
#include "reoscore.h"
#include "reosarea.h"


class REOSCORE_EXPORT ReosParameter : public QObject
{
    Q_OBJECT
  public:
    explicit ReosParameter( const QString &name, QObject *parent = nullptr );
    QString name() const;

    bool isDerivable() const;
    void setDerivable( bool b );
    bool isDerived() const;

    void askForDerivation()
    {
      emit needDerivation();
    }

  signals:
    void valueChanged();
    void needDerivation();

  protected:
    bool mIsDerived;
    void encode( ReosEncodedElement &element ) const;
    void decode( const ReosEncodedElement &element, bool isDerivable );

  private:
    QString mName;
    bool mIsDerivable;

};

class REOSCORE_EXPORT ReosParameterArea: public ReosParameter
{
  public:
    explicit ReosParameterArea( const QString &name, QObject *parent = nullptr );

    void setValue( const ReosArea &area );
    void setDerivedValue( const ReosArea &area );
    void changeUnit( ReosArea::Unit unit );
    ReosArea value() const {return mValue;}

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

    ReosEncodedElement encode() const;
    static ReosParameterSlope *decode( const ReosEncodedElement &element, bool isDerivable, QObject *parent );

  private:
    double mSlope = 0;
};


#endif // REOSPARAMETER_H
