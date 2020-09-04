/***************************************************************************
                      hdtineditoruidialog.h
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDTINEDITORUIDIALOG_H
#define HDTINEDITORUIDIALOG_H

#include <QDialog>
#include <QToolBar>
#include <QCloseEvent>

#include "../../Reos/reossettings.h"

class ReosTinEditorUi;

namespace Ui
{
  class HdTinEditorUiDialog;
}

class HdTinEditorUiDialog : public QDialog
{
    Q_OBJECT

  public:

    explicit HdTinEditorUiDialog( QWidget *parent = nullptr );
    ~HdTinEditorUiDialog() override;

    void setActions( const QList<QAction *> &actions );

    void setZSpecifierWidet( QWidget *widget );

    void setFocus();

    bool autoUpdate() const;


  signals :
    void closed();
    void escapePressed();

  public slots:
    void show()
    {
      if ( isVisible() )
        return;

      QDialog::show();

      ReosSettings settings;
      restoreGeometry( settings.value( QStringLiteral( "DelineateAutomaticDialog/geometry" ) ).toByteArray() );
    }

  protected:
    void keyPressEvent( QKeyEvent *event ) override
    {
      if ( event->key() == Qt::Key_Escape )
      {
        event->ignore();
        emit escapePressed();
      }
      else
      {
        QDialog::keyPressEvent( event );
      }
    }

  private slots:
    void updateSettings();

  private:
    Ui::HdTinEditorUiDialog *ui;
    QToolBar *toolbar;
    bool isClosed = true;


};

#endif // HDTINEDITORUIDIALOG_H
