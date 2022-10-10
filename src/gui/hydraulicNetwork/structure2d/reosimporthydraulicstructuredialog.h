/***************************************************************************
  reosimporthydraulicstructuredialog.h - ReosImportHydraulicStructureDialog

 ---------------------
 begin                : 7.10.2022
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
#ifndef REOSIMPORTHYDRAULICSTRUCTUREDIALOG_H
#define REOSIMPORTHYDRAULICSTRUCTUREDIALOG_H

#include <QDialog>
#include <QMap>
#include "reosgui.h"

class ReosHydraulicStructure2D;
class ReosHydraulicNetworkContext;
class ReosImportHydraulicStructureWidget;

namespace Ui
{
  class ReosImportHydraulicStructureDialog;
}

class ReosImportHydraulicStructureDialog : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosImportHydraulicStructureDialog( QWidget *parent = nullptr );
    ~ReosImportHydraulicStructureDialog();

    void createStructure2d( const ReosHydraulicNetworkContext &context ) const;

  private slots:
    void onEngineChanged();

  private:
    Ui::ReosImportHydraulicStructureDialog *ui;

    QMap<QString, QString> mEngines;
    ReosImportHydraulicStructureWidget *mCurrentEngineWidget = nullptr;
};


class REOSGUI_EXPORT ReosImportHydraulicStructureWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosImportHydraulicStructureWidget( QWidget *parent = nullptr )
      : QWidget( parent )
    {}

    virtual void importStructure2D( const ReosHydraulicNetworkContext &context ) const = 0;

};

#endif // REOSIMPORTHYDRAULICSTRUCTUREDIALOG_H
