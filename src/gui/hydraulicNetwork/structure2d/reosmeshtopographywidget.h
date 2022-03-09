/***************************************************************************
  reosmeshtopographywidget.h - ReosMeshTopographyWidget

 ---------------------
 begin                : 25.2.2022
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
#ifndef REOSMESHTOPOGRAPHYWIDGET_H
#define REOSMESHTOPOGRAPHYWIDGET_H

#include "reosactionwidget.h"
#include "reosguicontext.h"

class ReosMeshScalarRenderingWidget;
class ReosMesh;
class ReosTopographyCollection;
class ReosTopographyCollectionListModel;

namespace Ui
{
  class ReosMeshTopographyWidget;
}

class ReosMeshTopographyWidget: public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    ReosMeshTopographyWidget( ReosMesh *mesh,
                              ReosTopographyCollection *topographyCollection,
                              const QString &topographyDatasetId,
                              const ReosGuiContext &guiContext );

  public slots:
    void applyDem();

  private slots:
    void onAddTopography();
    void onRenderingSettings();
    void onMapCursorMove( const QPointF &pos );

  private:
    Ui::ReosMeshTopographyWidget *ui = nullptr;
    ReosMesh *mMesh = nullptr;
    QString mTopographyDatasetId;
    ReosGuiContext mGuiContext;
    ReosTopographyCollection *mTopographyCollection = nullptr;
    ReosTopographyCollectionListModel *mCollectionModel = nullptr;

};


class ReosMeshTopographyStackedWidget : public ReosActionStackedWidget
{
    Q_OBJECT

  public:
    explicit ReosMeshTopographyStackedWidget( ReosMesh *mesh, ReosTopographyCollection *topographyCollection, const QString &topographyDatasetId, const ReosGuiContext &guiContext );

};

#endif // REOSMESHTOPOGRAPHYWIDGET_H
