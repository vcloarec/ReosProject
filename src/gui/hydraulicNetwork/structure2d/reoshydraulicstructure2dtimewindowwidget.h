/***************************************************************************
  reoshydraulicstructure2dtimewindowwidget.h - Reoshydraulicstructure2dTimeWindowWidget

 ---------------------
 begin                : 2.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSTRUCTURE2DTIMEWINDOWWIDGET_H
#define REOSHYDRAULICSTRUCTURE2DTIMEWINDOWWIDGET_H

#include "reosactionwidget.h"

namespace Ui
{
  class Reoshydraulicstructure2dTimeWindowWidget;
}

class ReosTimeWindowSettings;

class Reoshydraulicstructure2dTimeWindowWidget : public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    explicit Reoshydraulicstructure2dTimeWindowWidget( ReosTimeWindowSettings *timeWindowSettings, QWidget *parent = nullptr );
    ~Reoshydraulicstructure2dTimeWindowWidget();

    void setExternallyDefinedEnable( bool enable );

  private slots:
    void onExternallyCheckBoxToogle();
    void onAutomaticGroupBoxToggle();
    void onOriginChange();
    void onCombineMethodChange();

    void syncTimeWindowSettings();

  private:
    Ui::Reoshydraulicstructure2dTimeWindowWidget *ui;
    ReosTimeWindowSettings *mTimeWindowSettings = nullptr;
    bool mExternalEnable = false;
};

#endif // REOSHYDRAULICSTRUCTURE2DTIMEWINDOWWIDGET_H
