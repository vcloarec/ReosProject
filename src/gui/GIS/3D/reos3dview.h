/***************************************************************************
  reos3dview.h - Reos3dView

 ---------------------
 begin                : 3.3.2022
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
#ifndef REOS3DVIEW_H
#define REOS3DVIEW_H

#include <reosactionwidget.h>

class Qgs3DMapCanvas;
class ReosMesh;

namespace Ui
{
  class Reos3dView;
}

class Reos3dView : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit Reos3dView( ReosMesh *mesh, QWidget *parent = nullptr );

    ~Reos3dView();

  private:
    Ui::Reos3dView *ui;

    Qgs3DMapCanvas *mCanvas = nullptr;
    QAction *mActionZoomExtent = nullptr;
};

#endif // REOS3DVIEW_H
