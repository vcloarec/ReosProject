/***************************************************************************
                      reoslanguageselectionwidget.h
                     --------------------------------------
Date                 : 21-08-2018
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

#ifndef REOSLANGUAGESELECTIONWIDGET_H
#define REOSLANGUAGESELECTIONWIDGET_H

#include <QDialog>
#include <QStringList>

namespace Ui
{
  class ReosLanguageSelectionWidget;
}

//! Widget to select language
class ReosLanguageSelectionWidget : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosLanguageSelectionWidget( QLocale locale, QWidget *parent = nullptr );
    ~ReosLanguageSelectionWidget();

    QLocale language();

  private:
    Ui::ReosLanguageSelectionWidget *ui;

    QStringList availableLanguages() const;
};

#endif // REOSLANGUAGESELECTIONWIDGET_H
