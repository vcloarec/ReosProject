/***************************************************************************
  reosnotificationbutton.h - ReosNotificationButton

 ---------------------
 begin                : 23.12.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSNOTIFICATIONBUTTON_H
#define REOSNOTIFICATIONBUTTON_H

#include <QToolButton>
#include <QTextBrowser>

#include "reosmodule.h"

class ReosNotificationButton : public QToolButton
{
  public:
    ReosNotificationButton( QWidget *parent );
    void setMessage( const ReosModule::Message &message );

  private:
    QTextBrowser *mTextBrowser = nullptr;
};

#endif // REOSNOTIFICATIONBUTTON_H