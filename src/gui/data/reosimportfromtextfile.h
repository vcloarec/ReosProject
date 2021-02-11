/***************************************************************************
  reosimportfromtextfile.h - ReosImportFromTextFile

 ---------------------
 begin                : 9.2.2021
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
#ifndef REOSIMPORTFROMTEXTFILE_H
#define REOSIMPORTFROMTEXTFILE_H

#include <QWidget>

#include "reosformwidget.h"

namespace Ui
{
  class ReosImportFromTextFile;
}

class ReosTextFileData;

class ReosImportFromTextFile : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosImportFromTextFile( ReosTextFileData *data, QWidget *parent = nullptr );
    ~ReosImportFromTextFile();

    QComboBox *createAvailableFieldComboBox( QWidget *parent );

  private:
    void onFileNameButton();
    void onFileNameEdited();
    void onHeaderLineChanged();
    void onFirstLineChanged();
    void onComboDelimitersChanged();
    void onAddDelimiters();
    void onRemoveDelimiters();

  private:
    Ui::ReosImportFromTextFile *ui;

    ReosTextFileData *mData;
};

#endif // REOSIMPORTFROMTEXTFILE_H
