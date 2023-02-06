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
#include "reosguicontext.h"

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
    explicit ReosImportHydraulicStructureDialog( const ReosGuiContext &context );
    ~ReosImportHydraulicStructureDialog();

    void createStructure2d( const ReosHydraulicNetworkContext &context ) const;

  private slots:
    void onEngineChanged();
    void onIsValidated( bool validated );

  private:
    Ui::ReosImportHydraulicStructureDialog *ui;
    const ReosGuiContext mGuiContext;

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

    // Create a new 2D structure and return a pointer to it
    virtual ReosHydraulicStructure2D *importStructure2D( const ReosHydraulicNetworkContext &context ) const = 0;

    virtual bool isValid() const = 0;

  signals:
    void isValidated( bool valid );

};

#endif // REOSIMPORTHYDRAULICSTRUCTUREDIALOG_H
