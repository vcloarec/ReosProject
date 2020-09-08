/***************************************************************************
                      reosaboutwidget.h
                     --------------------------------------
Date                 : 23-05-2018
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

#ifndef REOSABOUTWIDGET_H
#define REOSABOUTWIDGET_H

#include <QDialog>

#include "reosversion.h"

namespace Ui
{
  class ReosAboutWidget;
}

class ReosAboutWidget : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosAboutWidget( QWidget *parent = nullptr );
    ~ReosAboutWidget();

    void setSoftwarename( const QString &nom );
    void setBan( const QPixmap &pixmap );
    void setVersion( const QString &version );
    void setVersion( const ReosVersion &version );
    void setWebAddress( const QString &ad, const QString &from = QString() );
    void addLibrary( const QString &bibli, const QString &version, const QString &lienWEB = QString() );

    void setLicenceText( const QString &txt );

  private:
    Ui::ReosAboutWidget *ui;
};

#endif // REOSABOUTWIDGET_H
