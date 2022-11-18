/***************************************************************************
  reosrenderersettings_p.h - ReosRendererSettings_p

 ---------------------
 begin                : 16.11.2022
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
#ifndef REOSRENDERERSETTINGS_P_H
#define REOSRENDERERSETTINGS_P_H

#include <qgsmapsettings.h>
#include <qgsrasterlayer.h>

#include "reosrenderedobject.h"

class ReosRendererSettings_p : public ReosRendererSettings
{
  public:
    ReosRendererSettings_p( const void *settings );

    QDateTime mapTime() const override;

    const QgsMapSettings &settings() const;

  private:
    QgsMapSettings mSettings;
};

class ReosQgisLayerRenderer_p : public ReosObjectRenderer
{
  public:
    ReosQgisLayerRenderer_p( ReosRendererSettings *settings, QgsMapLayer *layer, ReosRenderedObject *renderedObject );
    ~ReosQgisLayerRenderer_p();

    void render() const override;
    bool isRenderingStopped() const override;

  protected:
    void stopRendering() override;

  private:
    std::unique_ptr<QgsMapLayerRenderer> mLayerRenderer;
    std::unique_ptr<QPainter> mPainter;
    QgsRenderContext mRenderContext;
};

class ReosRasterRenderer_p: public ReosObjectRenderer
{
  public:
    bool isRenderingStopped() const override;
    void render() const override;

  protected:
    void stopRendering() override;
};

#endif // REOSRENDERERSETTINGS_P_H
