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
#include <QSet>

#include "reoslanguageselectionwidget.h"
#include "ui_reoslanguageselectionwidget.h"

#include "reosapplication.h"

ReosLanguageSelectionWidget::ReosLanguageSelectionWidget( const QLocale &localeLanguage, const QLocale &globalLocale, QWidget *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosLanguageSelectionWidget )
{
  ui->setupUi( this );
  ui->comboBoxLanguage->addItem( tr( "System language" ), QLocale::system() );

  const QStringList languages = availableLanguages();
  for ( const QString &lang : languages )
    ui->comboBoxLanguage->addItem( QLocale( lang ).nativeLanguageName(), QLocale( lang ) );

  ui->comboBoxLanguage->setCurrentIndex( ui->comboBoxLanguage->findData( localeLanguage ) );

  const QStringList globalFormat = availableNumberLocal();
  for ( const QString &formatName : globalFormat )
  {
    QLocale l = QLocale( formatName );
    ui->comboBoxNumberFormat->addItem( QStringLiteral( "%1 %2 (%3)" ).arg( QLocale::languageToString( l.language() ), QLocale::countryToString( l.country() ), l.name() ), l );

  }

  ui->comboBoxNumberFormat->setCurrentIndex( ui->comboBoxNumberFormat->findData( globalLocale ) );

}

ReosLanguageSelectionWidget::~ReosLanguageSelectionWidget()
{
  delete ui;
}

QLocale ReosLanguageSelectionWidget::language() const
{
  if ( ui->comboBoxLanguage->currentText() < 0 )
    return QLocale::system();

  return ui->comboBoxLanguage->currentData().toLocale();
}

QLocale ReosLanguageSelectionWidget::global() const
{
  if ( ui->comboBoxNumberFormat->currentText() < 0 )
    return QLocale::system();

  return ui->comboBoxNumberFormat->currentData().toLocale();
}

QStringList ReosLanguageSelectionWidget::availableLanguages() const
{
  //From QGIS, QgsOptions
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

QStringList ReosLanguageSelectionWidget::availableNumberLocal() const
{
  //From QGIS, QgsOptions
  const QList<QLocale> allLocales = QLocale::matchingLocales(
                                      QLocale::AnyLanguage,
                                      QLocale::AnyScript,
                                      QLocale::AnyCountry );

  QSet<QString> addedLocales;
  QStringList globalLocales;
  for ( const auto &l : allLocales )
  {
    // Do not add duplicates (like en_US)
    if ( ! addedLocales.contains( l.name() ) )
    {
      globalLocales.append( l.name() );
      addedLocales.insert( l.name() );
    }
  }

  return globalLocales;
}
