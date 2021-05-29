/***************************************************************************
                      reosmessagebox.h
                     --------------------------------------
Date                 : 30-12-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSMESSAGEBOX_H
#define REOSMESSAGEBOX_H

#include <QWidget>
#include <QTime>

#include "reosmodule.h"

namespace Ui
{
  class ReosMessageBox;
}

//! Widget displaying messages for the user
class ReosMessageBox : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosMessageBox( QWidget *parent = nullptr );
    ~ReosMessageBox();

  public slots:
    void receiveMessage( const QString &mes, ReosModule::MessageType type = ReosModule::Message, bool messageBox = false );
    void clean();

  private:
    Ui::ReosMessageBox *ui;
};

#endif // REOSMESSAGEBOX_H
