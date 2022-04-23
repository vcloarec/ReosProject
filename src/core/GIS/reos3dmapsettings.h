/***************************************************************************
  reos3dmapsettings.h - Reos3DMapSettings

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
#ifndef REOS3DMAPSETTINGS_H
#define REOS3DMAPSETTINGS_H

#include <QVector3D>
#include <QColor>
#include "reoscore.h"

class ReosEncodedElement;

class REOSCORE_EXPORT Reos3DMapSettings
{
  public:
    Reos3DMapSettings();
    Reos3DMapSettings( const ReosEncodedElement &element );

    double verticalExaggeration() const;
    void setVerticalExaggeration( double verticalExaggeration );

    QVector3D lightDirection() const;
    void setLightDirection( const QVector3D &lightDirection );

    double lightIntensity() const;
    void setLightIntensity( double lightIntensity );

    ReosEncodedElement encode() const;

  private:
    QVector3D mLightDirection = { -0.32, -0.91, -0.27 };
    double mLightIntensity = 1;
    double mVerticalExaggeration = 1;
};


class REOSCORE_EXPORT Reos3DTerrainSettings
{
  public:
    enum RenderingType
    {
      UniqueColor,
      ColorRamp,
    };

    Reos3DTerrainSettings();
    Reos3DTerrainSettings( const ReosEncodedElement &element );

    ReosEncodedElement encode() const;

    QColor uniqueColor() const;
    void setUniqueColor( const QColor &uniqueColor );

    RenderingType renderingType() const;
    void setRenderingType( const RenderingType &renderingType );

    bool isWireframeEnabled() const;
    void setIsWireframeEnabled( bool enableWireframe );

    double wireframeWidth() const;
    void setWireframeWidth( double wireframeWidth );

    QColor wireframeColor() const;
    void setWireframeColor( const QColor &wireframeColor );

    bool isSmoothed() const;
    void setIsSmoothed( bool isSmoothed );

  private:
    QColor mUniqueColor = Qt::darkGreen;
    RenderingType mRenderingType = UniqueColor;
    bool mEnableWireframe = true;
    double mWireframeWidth = 0.2;
    QColor mWireframeColor = Qt::white;
    bool mIsSmoothed = false;
};

#endif // REOS3DMAPSETTINGS_H
