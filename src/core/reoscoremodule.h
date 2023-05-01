/***************************************************************************
  reoscoremodule.h - ReosCoreModule

 ---------------------
 begin                : 11.3.2023
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
#ifndef REOSCOREMODULE_H
#define REOSCOREMODULE_H

#include "reosmodule.h"

class ReosWatershedModule;
class ReosGisEngine;
class ReosHydraulicNetwork;


class REOSCORE_EXPORT ReosCoreModule : public ReosModule
{
    Q_OBJECT
  public:

    //! Constructor
    ReosCoreModule( QObject *parent = nullptr );

    //! Returns a pointer to the GIS engine
    ReosGisEngine *gisEngine() const;

    //! Opens a project with file path \a filePath
    bool openProject( const QString &filePath );

    //! Saves project with a file path \a filePath
    bool saveProject( const QString &filePath );

    //! Clears all data on the current project
    void clearProject();

    //! Returns a pointer to the hydraulic network module
    ReosHydraulicNetwork *hydraulicNetwork() const;

    //! Returns a pointer to the watersehd module
    ReosWatershedModule *watershedModule() const;
};


#endif // REOSCOREMODULE_H
