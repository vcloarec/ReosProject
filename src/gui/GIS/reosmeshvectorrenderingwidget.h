/***************************************************************************
  reosmeshvectorrenderingwidget.h - ReosMeshVectorRenderingWidget

 ---------------------
 begin                : 8.5.2022
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
#ifndef REOSMESHVECTORRENDERINGWIDGET_H
#define REOSMESHVECTORRENDERINGWIDGET_H


#include "reosactionwidget.h"
#include "reosguicontext.h"

namespace Ui
{
  class ReosMeshVectorRenderingWidget;
}

class ReosMesh;
class ReosGuiContext;
class QgsMeshRendererVectorSettings;
class ReosParameterDouble;

class ReosMeshVectorRenderingWidget : public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    explicit ReosMeshVectorRenderingWidget( ReosMesh *mesh, const QString &datasetId, const ReosGuiContext &guiContext );
    ~ReosMeshVectorRenderingWidget();

  private slots:
    void updateMeshSettings();

  private:
    Ui::ReosMeshVectorRenderingWidget *ui;
    ReosMesh *mMesh = nullptr;
    QString mDatasetId;
    ReosGuiContext mGuiContext;
    ReosParameterDouble *mWidthParameter = nullptr;
    ReosParameterDouble *mMinimumLengthParameter = nullptr;
    ReosParameterDouble *mMaximumLengthParameter = nullptr;
    ReosParameterDouble *mMaximumTailLengthParameter = nullptr;

    void setSettings( const QgsMeshRendererVectorSettings &settings );
    void updateWidget();
    QgsMeshRendererVectorSettings vectorSettings();
};

#endif // REOSMESHVECTORRENDERINGWIDGET_H
