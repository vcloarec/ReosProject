/***************************************************************************
  reosnetworkcompatibilitydialog.h - ReosNetworkCompatibilityDialog

 ---------------------
 begin                : 21.1.2023
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
#ifndef REOSNETWORKCOMPATIBILITYDIALOG_H
#define REOSNETWORKCOMPATIBILITYDIALOG_H

#include <QDialog>
#include "reosgui.h"

namespace Ui
{
  class ReosNetworkCompatibilityDialog;
}

struct ReosHydraulicNetworkElementCompatibilty;
class ReosGuiContext;

class REOSGUI_EXPORT ReosNetworkCompatibilityDialog : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosNetworkCompatibilityDialog(
      const QString &introText,
      const ReosHydraulicNetworkElementCompatibilty compatibility,
      const QString &finalText,
      const ReosGuiContext &context );
    ~ReosNetworkCompatibilityDialog();

  private:
    Ui::ReosNetworkCompatibilityDialog *ui;
};

#endif // REOSNETWORKCOMPATIBILITYDIALOG_H
