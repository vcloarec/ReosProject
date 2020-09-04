/***************************************************************************
                      reosduration.h
                     --------------------------------------
Date                 : 21-08-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                :   projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef REOSDURATION_H
#define REOSDURATION_H

#include <string>
#include <sstream>
#include <iostream>
#include <math.h>

//#include "../../cmn/Fichier/hdenregistrement.h"
#include "../reosencodedelement.h"

class ReosDuration
{
  public:

    enum Unit {seconde, minute, heure, jour, semaine, mois, annee};

    ReosDuration( double valeur = 0, Unit mUnit = seconde );
    ReosDuration( bool defined ): mDefined( defined )
    {}
    ReosDuration( const ReosEncodedElement &encoded )
    {
      qint8 u;
      if ( encoded.getData( QStringLiteral( "Unit" ), u ) )
        mUnit = Unit( u );
      qreal val;
      if ( encoded.getData( QStringLiteral( "Value" ), val ) )
        mValueInSeconde = val;
    }

    std::string toString( int precision = 2 ) const;

    ReosDuration operator+( const ReosDuration & ) const;
    ReosDuration operator-( const ReosDuration & ) const;
    ReosDuration operator*( double k ) const;
    ReosDuration operator*( int i ) const;
    ReosDuration operator/( double k ) const;
    double operator/( ReosDuration & ) const;
    bool operator>( const ReosDuration & ) const;
    bool operator>=( const ReosDuration & ) const;
    bool operator<( const ReosDuration & ) const;
    bool operator<=( const ReosDuration & ) const;
    bool operator==( const ReosDuration & ) const;
    bool operator!=( const ReosDuration & ) const;

    double getValueSeconde() const;
    double getValueMinute() const;
    double getValueHour() const;
    double getValueDay() const;
    double getValueWeek() const;
    double getValueMonth() const;
    double getValueYear() const;
    double getValueUnit() const;
    double getValueUnit( Unit un ) const;

    Unit unit() const;
    void setUnit( Unit u );
    [[deprecated]] QByteArray encodage() const ;
    QByteArray encode() const;
    QString getString( ReosDuration::Unit unit = ReosDuration::heure, int precision = 1 );

    unsigned numberOfFullyContainedIntervals( const ReosDuration &other ) const;

    bool isDefined() const
    {
      return mDefined;
    }

  private:
    Unit mUnit = seconde;
    double mValueInSeconde = 0;
    bool mDefined = true;
};


[[deprecated]] ReosDuration decodeDuration( const QByteArray &byteArray );

#endif //REOSDURATION_H
