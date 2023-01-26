/***************************************************************************
  reosdssprovideruriwidget.h - ReosDssProviderUriWidget

 ---------------------
 begin                : 25.1.2023
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
#ifndef REOSDSSPROVIDERURIWIDGET_H
#define REOSDSSPROVIDERURIWIDGET_H

#include <QWidget>

#include "reosdataprovidergui.h"

class ReosDssPath;

namespace Ui
{
  class ReosDssProviderUriWidget;
}

class ReosDssProviderUriWidget : public ReosDataProviderUriWidget
{
    Q_OBJECT

  public:
    explicit ReosDssProviderUriWidget( QWidget *parent = nullptr );
    ~ReosDssProviderUriWidget();

    void setUri( const QString &uri );
    const QString uri() const;

    void setDataType( const QString &dataType ) override;

    void setPath( const ReosDssPath &path );

  private slots:
    void onFileButtonPressed();

  private:
    Ui::ReosDssProviderUriWidget *ui;


};

#endif // REOSDSSPROVIDERURIWIDGET_H
