/***************************************************************************
  reosmeshscalarrenderingwidget.h - ReosMeshScalarRenderingWidget

 ---------------------
 begin                : 24.2.2022
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
#ifndef REOSMESHSCALARRENDERINGWIDGET_H
#define REOSMESHSCALARRENDERINGWIDGET_H


#include "reosactionwidget.h"
#include "reosmesh.h"
#include "reosguicontext.h"
#include "reosparameter.h"

namespace Ui
{
  class ReosMeshScalarRenderingWidget;
}

class QgsColorRampShaderWidget;
class QgsMeshLayer;
class ReosColorShaderSettings;


class ReosMeshScalarRenderingWidget: public ReosStackedPageWidget
{
  public:
    ReosMeshScalarRenderingWidget( ReosColorShaderSettings *settings, const ReosGuiContext &guiContext );
    ~ReosMeshScalarRenderingWidget();

  private slots:
    void onMinMaxChanged();
    void onColorRampChanged();

    void syncSettings();

  private:
    Ui::ReosMeshScalarRenderingWidget *ui;
    QPointer<ReosColorShaderSettings> mSettings;
    ReosMesh *mMesh = nullptr;
    QString mDatasetId;
    bool mIsScalar;
    ReosParameterDouble *mMinimumParam = nullptr;
    ReosParameterDouble *mMaximumParam = nullptr;

    QgsColorRampShaderWidget *mColorRampShaderWidget = nullptr;

    void updateSettings();
};

#endif // REOSMESHSCALARRENDERINGWIDGET_H
