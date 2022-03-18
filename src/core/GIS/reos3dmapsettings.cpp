/***************************************************************************
  reos3dmapsettings.cpp - Reos3DMapSettings

 ---------------------
 begin                : 15.3.2022
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
#include "reos3dmapsettings.h"

#include "reosencodedelement.h"

Reos3DMapSettings::Reos3DMapSettings()
{}


ReosEncodedElement Reos3DMapSettings::encode() const
{
  ReosEncodedElement element( QStringLiteral( "3d-map-settings" ) );

  element.addData( QStringLiteral( "light-direction" ), mLightDirection );
  element.addData( QStringLiteral( "light-intensity" ), mLightIntensity );
  element.addData( QStringLiteral( "vertical-exaggeration" ), mVerticalExaggeration );

  return element;
}

double Reos3DMapSettings::lightIntensity() const
{
  return mLightIntensity;
}

void Reos3DMapSettings::setLightIntensity( double lightIntensity )
{
  mLightIntensity = lightIntensity;
}

QVector3D Reos3DMapSettings::lightDirection() const
{
  return mLightDirection;
}

void Reos3DMapSettings::setLightDirection( const QVector3D &lightDirection )
{
  mLightDirection = lightDirection;
}

Reos3DMapSettings::Reos3DMapSettings( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "3d-map-settings" ) )
    return;

  element.getData( QStringLiteral( "light-direction" ), mLightDirection );
  element.getData( QStringLiteral( "light-intensity" ), mLightIntensity );
  element.getData( QStringLiteral( "vertical-exaggeration" ), mVerticalExaggeration );
}

double Reos3DMapSettings::verticalExaggeration() const
{
  return mVerticalExaggeration;
}

void Reos3DMapSettings::setVerticalExaggeration( double verticalExaggeration )
{
  mVerticalExaggeration = verticalExaggeration;
}

Reos3DTerrainSettings::Reos3DTerrainSettings() {}

Reos3DTerrainSettings::Reos3DTerrainSettings( const ReosEncodedElement &element )
{
  element.getData( QStringLiteral( "unique-color" ), mUniqueColor );
  int type = 0;
  element.getData( QStringLiteral( "rendering-type" ), type );
  mRenderingType = static_cast<RenderingType>( type );
  element.getData( QStringLiteral( "enable-wireframe" ), mEnableWireframe );
  element.getData( QStringLiteral( "wire-frame-width" ), mWireframeWidth );
  element.getData( QStringLiteral( "wire-frame-color" ), mWireframeColor );
  element.getData( QStringLiteral( "is-smoothed" ), mIsSmoothed );
}

ReosEncodedElement Reos3DTerrainSettings::encode() const
{
  ReosEncodedElement element( QStringLiteral( "terrain-3d-settings" ) );

  element.addData( QStringLiteral( "unique-color" ), mUniqueColor );
  element.addData( QStringLiteral( "rendering-type" ), int( mRenderingType ) );
  element.addData( QStringLiteral( "enable-wireframe" ), mEnableWireframe );
  element.addData( QStringLiteral( "wire-frame-width" ), mWireframeWidth );
  element.addData( QStringLiteral( "wire-frame-color" ), mWireframeColor );
  element.addData( QStringLiteral( "is-smoothed" ), mIsSmoothed );

  return element;
}

QColor Reos3DTerrainSettings::uniqueColor() const
{
  return mUniqueColor;
}

void Reos3DTerrainSettings::setUniqueColor( const QColor &uniqueColor )
{
  mUniqueColor = uniqueColor;
}

Reos3DTerrainSettings::RenderingType Reos3DTerrainSettings::renderingType() const
{
  return mRenderingType;
}

void Reos3DTerrainSettings::setRenderingType( const RenderingType &renderingType )
{
  mRenderingType = renderingType;
}

bool Reos3DTerrainSettings::isWireframeEnabled() const
{
  return mEnableWireframe;
}

void Reos3DTerrainSettings::setIsWireframeEnabled( bool enableWireframe )
{
  mEnableWireframe = enableWireframe;
}

double Reos3DTerrainSettings::wireframeWidth() const
{
  return mWireframeWidth;
}

void Reos3DTerrainSettings::setWireframeWidth( double wireframeWidth )
{
  mWireframeWidth = wireframeWidth;
}

QColor Reos3DTerrainSettings::wireframeColor() const
{
  return mWireframeColor;
}

void Reos3DTerrainSettings::setWireframeColor( const QColor &wireframeColor )
{
  mWireframeColor = wireframeColor;
}

bool Reos3DTerrainSettings::isSmoothed() const
{
  return mIsSmoothed;
}

void Reos3DTerrainSettings::setIsSmoothed( bool isSmoothed )
{
  mIsSmoothed = isSmoothed;
}
