/***************************************************************************
  reoseditstructure2dwidget.h - ReosEditStructure2DWidget

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSEDITSTRUCTURE2DWIDGET_H
#define REOSEDITSTRUCTURE2DWIDGET_H

#include <QWidget>

#include "reosactionwidget.h"

namespace Ui {
class ReosEditStructure2DWidget;
}

class ReosEditStructure2DWidget : public ReosStackedPageWidget
{
    Q_OBJECT

public:
    explicit ReosEditStructure2DWidget(QWidget *parent = nullptr);
    ~ReosEditStructure2DWidget();

private:
    Ui::ReosEditStructure2DWidget *ui;

    QAction *mActionEditLine=nullptr;

};

#endif // REOSEDITSTRUCTURE2DWIDGET_H
