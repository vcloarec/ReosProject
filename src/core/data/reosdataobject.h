/***************************************************************************
  reosdataobject.h - ReosDataObject

 ---------------------
 begin                : 4.2.2021
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
#ifndef REOSDATAOBJECT_H
#define REOSDATAOBJECT_H

#include <QObject>


//! Abstract class uses to be an interface for data
class ReosDataObject: public QObject
{
    Q_OBJECT
  public:
    ReosDataObject( QObject *parent = nullptr );

    //! Returns the type
    virtual QString type() const = 0;

    //! Returns the name of the data object
    QString name() const;

  public slots:
    //! Sets the name of the data object
    void setName( const QString &name );

  signals:
    void dataChanged();
    void settingsChanged();

  private:
    QString mName;
};
#endif // REOSDATAOBJECT_H
