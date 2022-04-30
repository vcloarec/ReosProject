/***************************************************************************
  reostelemac2dinitialcondition.cpp - ReosTelemac2DInitialCondition

 ---------------------
 begin                : 28.4.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostelemac2dinitialcondition.h"


#include "reosparameter.h"

ReosTelemac2DInitialCondition::ReosTelemac2DInitialCondition( QObject *parent )
  : ReosDataObject( parent )
{
}

ReosTelemac2DInitialCondition::ReosTelemac2DInitialCondition( const ReosEncodedElement &element, QObject *parent )
  : ReosDataObject( parent )
{
  ReosDataObject::decode( element );
}

ReosParameterDouble *ReosTelemac2DInitialConstantWaterLevel::initialWaterLevel() const
{
  return mInitialWaterLevel;
}

ReosTelemac2DInitialConstantWaterLevel::ReosTelemac2DInitialConstantWaterLevel( QObject *parent )
  : ReosTelemac2DInitialCondition( parent )
{
  mInitialWaterLevel = new ReosParameterDouble( tr( "Initial water level" ), false, this );
  mInitialWaterLevel->setValue( 4 );
}

ReosTelemac2DInitialConstantWaterLevel::ReosTelemac2DInitialConstantWaterLevel( const ReosEncodedElement &element, QObject *parent )
  : ReosTelemac2DInitialCondition( element, parent )
{
  mInitialWaterLevel = ReosParameterDouble::decode( element.getEncodedData(
                         QStringLiteral( "initial-water-level" ) ),
                       false, tr( "Initial water level" ), this );
}

ReosEncodedElement ReosTelemac2DInitialConstantWaterLevel::encode() const
{
  ReosEncodedElement element( QStringLiteral( "intital-conditions" ) );
  element.addEncodedData( QStringLiteral( "initial-water-level" ), mInitialWaterLevel->encode() );

  ReosDataObject::encode( element );

  return element;
}
