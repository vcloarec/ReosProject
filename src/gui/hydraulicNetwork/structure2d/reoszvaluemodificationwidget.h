/***************************************************************************
  reoszvaluemodificationwidget.h - ReosZValueModificationWidget

 ---------------------
 begin                : 11.3.2022
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
#ifndef REOSZVALUEMODIFICATIONWIDGET_H
#define REOSZVALUEMODIFICATIONWIDGET_H

#include <QDialog>

class ReosParameterDouble;

namespace Ui
{
  class ReosZValueModificationWidget;
}

class ReosZValueModificationWidget : public QDialog
{
    Q_OBJECT

  public:

    enum ModificationType
    {
      NewValue,
      Offset
    };

    explicit ReosZValueModificationWidget( QWidget *parent = nullptr );
    ~ReosZValueModificationWidget();

    ModificationType modificationType() const;

    double value();

  private:
    Ui::ReosZValueModificationWidget *ui;
    ReosParameterDouble *mValueParameter = nullptr;
};

#endif // REOSZVALUEMODIFICATIONWIDGET_H
