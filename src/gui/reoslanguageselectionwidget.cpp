/***************************************************************************
                      reoslanguageselectionwidget.cpp
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

#include <QDir>

#include "reoslanguageselectionwidget.h"
#include "ui_reoslanguageselectionwidget.h"

#include "reosapplication.h"

ReosLanguageSelectionWidget::ReosLanguageSelectionWidget( QLocale locale, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosLanguageSelectionWidget )
{
  ui->setupUi( this );
  ui->comboBox->addItem( tr( "System language" ), QLocale::system() );

  const QStringList languages = availableLanguages();
  for ( const QString &lang : languages )
    ui->comboBox->addItem( QLocale( lang ).nativeLanguageName(), QLocale( lang ) );

  ui->comboBox->setCurrentIndex( ui->comboBox->findData( locale ) );

}

ReosLanguageSelectionWidget::~ReosLanguageSelectionWidget()
{
  delete ui;
}

QLocale ReosLanguageSelectionWidget::language()
{
  if ( ui->comboBox->currentText() < 0 )
    return QLocale::system();

  return ui->comboBox->currentData().toLocale();
}

QStringList ReosLanguageSelectionWidget::availableLanguages() const
{
  //From QGIS, QgsOptions::i18nList()
  QStringList languageList;
  languageList << QStringLiteral( "en_US" ); //there is no qm file for this so we add it manually
  QString myI18nPath = ReosApplication::i18nPath();
  QDir myDir( myI18nPath, QStringLiteral( "reos*.qm" ) );
  QStringList myFileList = myDir.entryList();
  QStringListIterator myIterator( myFileList );
  while ( myIterator.hasNext() )
  {
    QString myFileName = myIterator.next();

    // Ignore the 'en' translation file, already added as 'en_US'.
    if ( myFileName.compare( QLatin1String( "reos_en.qm" ) ) == 0 ) continue;

    languageList << myFileName.remove( QStringLiteral( "reos_" ) ).remove( QStringLiteral( ".qm" ) );
  }
  return languageList;
}
