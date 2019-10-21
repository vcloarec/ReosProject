/***************************************************************************
                      reosdilaogbox.h
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSDIALOGBOX_H
#define REOSDIALOGBOX_H

#include <QDialog>
#include <QDialogButtonBox>
#include <QVBoxLayout>


class ReosDialogBox: public QDialog
{
  public:
    ReosDialogBox( QWidget *w, QWidget *parent = nullptr );

  private:

    QVBoxLayout *mLayout;

};

#endif // REOSDIALOGBOX_H
