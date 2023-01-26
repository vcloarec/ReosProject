/***************************************************************************
  reoscoordinatesystemwidget.h - ReosCoordinateSystemWidget

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
#ifndef REOSCOORDINATESYSTEMWIDGET_H
#define REOSCOORDINATESYSTEMWIDGET_H

#include <QWidget>

class QgsProjectionSelectionWidget;

class ReosCoordinateSystemWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosCoordinateSystemWidget( QWidget *parent = nullptr );

    QString crs() const;
    void setCrs( const QString &crs );

    int bestDecimalNumber() const;
    bool isGeographic() const;

  signals:
    void crsChanged();

  private:

    QgsProjectionSelectionWidget *mQgsWidget = nullptr;
};

#endif // REOSCOORDINATESYSTEMWIDGET_H
