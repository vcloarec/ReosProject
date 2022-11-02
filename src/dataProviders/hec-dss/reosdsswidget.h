/***************************************************************************
  reosdsswidget.h - ReosDssWidget

 ---------------------
 begin                : 22.10.2022
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
#ifndef REOSDSSWIDGET_H
#define REOSDSSWIDGET_H

#include <QWidget>

namespace Ui {
class ReosDssWidget;
}

class ReosDssWidget : public QWidget
{
    Q_OBJECT

public:
    explicit ReosDssWidget(QWidget *parent = nullptr);
    ~ReosDssWidget();

private:
    Ui::ReosDssWidget *ui;
};

#endif // REOSDSSWIDGET_H
