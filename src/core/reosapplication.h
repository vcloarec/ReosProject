/***************************************************************************
                      reosapplication.h
                     --------------------------------------
Date                 : 20-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSAPPLICATION_H
#define REOSAPPLICATION_H

#define SIP_NO_FILE

#include <QApplication>
#include <QPointer>

#include "reoscore.h"

class ReosCoreModule;

class REOSCORE_EXPORT ReosApplication: public QApplication
{
    Q_OBJECT
  public:
    ReosApplication( int &argc, char **argv, int flag = ApplicationFlags );
    ~ReosApplication();
    bool notify( QObject *receiver, QEvent *event ) override;

    static QString i18nPath();
    static QString styleSheet();

    ReosCoreModule *coreModule() const;

  private:
    QPointer<ReosCoreModule> mCoreModule;
};


#endif // REOSAPPLICATION_H
