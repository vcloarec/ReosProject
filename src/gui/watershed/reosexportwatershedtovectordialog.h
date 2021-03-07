/***************************************************************************
  reosexportwatershedtovectordialog.h - ReosExportWatershedToVectorDialog

 ---------------------
 begin                : 6.3.2021
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
#ifndef REOSEXPORTWATERSHEDTOVECTORDIALOG_H
#define REOSEXPORTWATERSHEDTOVECTORDIALOG_H

#include <QDialog>

class ReosWatershed;

namespace Ui
{
  class ReosExportWatershedToVectorDialog;
}

class ReosExportWatershedToVectorDialog : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosExportWatershedToVectorDialog( const QList<ReosWatershed *> &watersheds, const QString &crs, QWidget *parent = nullptr );
    ~ReosExportWatershedToVectorDialog();

  private slots:
    void onChooseDelineatingFile();
    void onChooseStreamPathFile();
    void accept();

  private:
    Ui::ReosExportWatershedToVectorDialog *ui;
    const QList<ReosWatershed *> mWatersheds;
    QString mCrs;

    bool checkFileExist( const QString &fileName );
};

#endif // REOSEXPORTWATERSHEDTOVECTORDIALOG_H
