/***************************************************************************
  reoseditmeshelementwidget.h - ReosEditMeshElementWidget

 ---------------------
 begin                : 9.3.2022
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
#ifndef REOSEDITMESHELEMENTWIDGET_H
#define REOSEDITMESHELEMENTWIDGET_H

#include <QWidget>
#include <QPointer>

#include "reosguicontext.h"

namespace Ui
{
  class ReosEditMeshElementWidget;
}

class ReosMesh;
class ReosMapToolEditMeshFrame;
class QToolBar;


class ReosEditMeshElementWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosEditMeshElementWidget( ReosMesh *mesh, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosEditMeshElementWidget();

  private:
    Ui::ReosEditMeshElementWidget *ui;
    QPointer<ReosMesh> mMesh;
    ReosGuiContext mGuiContext;
    QToolBar *mToolBar = nullptr;
    QAction *mActionEditMeshFrame = nullptr;
    ReosMapToolEditMeshFrame *mMapToolEditMeshFrame = nullptr;
};

#endif // REOSEDITMESHELEMENTWIDGET_H
