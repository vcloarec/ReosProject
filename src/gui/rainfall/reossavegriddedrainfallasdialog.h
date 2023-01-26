/***************************************************************************
  reossavegriddedrainfallasdialog.h - ReosSaveGriddedRainfallAsDialog

 ---------------------
 begin                : 16.1.2023
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
#ifndef REOSSAVEGRIDDEDRAINFALLASDIALOG_H
#define REOSSAVEGRIDDEDRAINFALLASDIALOG_H

#include <QDialog>

class ReosGriddedRainfall;
class ReosGuiContext;
class ReosDataProvider;
class ReosDataProviderUriWidget;

namespace Ui
{
  class ReosSaveGriddedRainfallAsDialog;
}

class ReosSaveGriddedRainfallAsDialog : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosSaveGriddedRainfallAsDialog( ReosGriddedRainfall *rainfall, const ReosGuiContext &context );
    ~ReosSaveGriddedRainfallAsDialog();

    void saveAs( ReosGriddedRainfall *rainfall ) const;

  protected:
    void keyPressEvent( QKeyEvent *evt ) override;

  private slots:
    void onFormatChanged();

  private:
    Ui::ReosSaveGriddedRainfallAsDialog *ui;

    QString mCurrentProviderKey;
    std::unique_ptr<ReosDataProvider> currentFormatProvider() const;
    ReosDataProviderUriWidget *mUriWidget = nullptr;

};

#endif // REOSSAVEGRIDDEDRAINFALLASDIALOG_H
