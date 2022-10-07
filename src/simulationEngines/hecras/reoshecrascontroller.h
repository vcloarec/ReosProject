/***************************************************************************
  reoshecrascontroller.h - ReosHecRasController

 ---------------------
 begin                : 03.10.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHECRASCONTROLLER_H
#define REOSHECRASCONTROLLER_H

#include <combaseapi.h>

#undef min
#undef max

#include <QPolygon>

class ReosHecrasController
{
public:
	//! Constructor with \a version of HecRas, \see availableVersion()
	ReosHecrasController(const QString& version);
	~ReosHecrasController();

	//! Returns available version of HecRas controller
	static QStringList availableVersion();

	//! Returns whether the controller is valid
	bool isValid() const;

	//! Returns the user-friendly string of the controller version of this instance
	QString version() const;

	//! Opens a project with path \a projFileName
	bool openHecrasProject(const QString& projFileName);

	//! Returns the plan names of the currently opened project
	QStringList planNames() const;

	//! Returns the flow 2D area names of the currently opened project
	QStringList flowAreas2D() const;

	//! Returns the domain of the flow area with \a areaName
	QPolygonF flow2DAreasDomain(const QString &areaName);

private:
	bool mIsValid = false;
	IDispatch* mDispatch = nullptr;
	QMap<QString, DISPID> mFunctionNames;

	bool exitRas() const;
};


#endif // REOSHECRASCONTROLLER_H
