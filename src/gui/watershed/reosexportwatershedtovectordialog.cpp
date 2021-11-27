/***************************************************************************
  reosexportwatershedtovectordialog.cpp - ReosExportWatershedToVectorDialog

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
#include "reosexportwatershedtovectordialog.h"
#include "ui_reosexportwatershedtovectordialog.h"

#include <QFileDialog>
#include <QMessageBox>

#include "reosexporttovectorfile.h"
#include "reoswatershed.h"


ReosExportWatershedToVectorDialog::ReosExportWatershedToVectorDialog( const QList<ReosWatershed *> &watersheds, const QString &crs, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosExportWatershedToVectorDialog ),
  mWatersheds( watersheds ),
  mCrs( crs )
{
  ui->setupUi( this );
  connect( ui->toolButtonChooseDelineatingFile, &QAbstractButton::clicked, this, &ReosExportWatershedToVectorDialog::onChooseDelineatingFile );
  connect( ui->toolButtonChooseStreamPathFile, &QAbstractButton::clicked, this, &ReosExportWatershedToVectorDialog::onChooseStreamPathFile );
  connect( ui->buttonBox, &QDialogButtonBox::accepted, this, &QDialog::accept );
}

ReosExportWatershedToVectorDialog::~ReosExportWatershedToVectorDialog()
{
  delete ui;
}

void ReosExportWatershedToVectorDialog::onChooseDelineatingFile()
{
  QString fileName = QFileDialog::getSaveFileName( nullptr, tr( "Export watershed delineating" ),
                     "", "*.shp" );

  ui->lineEditDelineatingFile->setText( fileName );

}

void ReosExportWatershedToVectorDialog::onChooseStreamPathFile()
{
  QString fileName = QFileDialog::getSaveFileName( nullptr, tr( "Export watershed longest path" ),
                     "", "*.shp" );

  ui->lineEditStreamPathFile->setText( fileName );
}

void ReosExportWatershedToVectorDialog::accept()
{
  if ( ui->groupBoxDelineating->isChecked() )
  {
    QList<ReosExportToVectorFile::Field> fields;
    fields.append( {tr( "Name" ), QVariant::String, "string", 255} );
    fields.append( {tr( "Area" ), QVariant::Double, "double", 16} );
    ReosExportToVectorFile exportToFile( ui->lineEditDelineatingFile->text(), fields, ReosExportToVectorFile::Polygon, mCrs );

    for ( ReosWatershed *ws : mWatersheds )
    {
      QVariantMap attributes;
      attributes[tr( "Name" )] = ws->watershedName()->value();
      attributes[tr( "Area" )] = ws->area()->value().valueM2();
      exportToFile.addPolygon( ws->delineating(), attributes );
    }
  }

  if ( ui->groupBoxPath->isChecked() )
  {
    QList<ReosExportToVectorFile::Field> fields;
    fields.append( {tr( "Name" ), QVariant::String, "string", 255} );
    fields.append( {tr( "Length" ), QVariant::Double, "double", 16} );
    fields.append( {tr( "Slope" ), QVariant::Double, "double", 16} );
    fields.append( {tr( "Drop" ), QVariant::Double, "double", 16} );
    ReosExportToVectorFile exportToFile( ui->lineEditStreamPathFile->text(), fields, ReosExportToVectorFile::Polyline, mCrs );

    for ( ReosWatershed *ws : mWatersheds )
    {
      QVariantMap attributes;
      attributes[tr( "Name" )] = ws->watershedName()->value();
      attributes[tr( "Length" )] = ws->longestPath()->value();
      attributes[tr( "Slope" )] = ws->slope()->value();
      attributes[tr( "Drop" )] = ws->drop()->value();
      exportToFile.addPolyline( ws->streamPath(), attributes );
    }
  }


  QDialog::accept();
}
