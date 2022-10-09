/***************************************************************************
  reoshecrassimulation.cpp - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
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

#include "reoshecrassimulation.h"
#include "reoscore.h"

REOSEXTERN ReosSimulationEngineFactory* engineSimulationFactory()
{
	return new ReosHecRasSimulationEngineFactory();
}

void ReosHecRasSimulationEngineFactory::initializeSettings()
{
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter(const QString& version, const QString& file)
	: ReosStructureImporter()
	, mController(version)
{
	mIsValid = mController.isValid();
	if (mIsValid)
		mIsValid=mController.openHecrasProject(file);
}

ReosHecRasStructureImporter::~ReosHecRasStructureImporter() = default;


ReosHydraulicStructure2D::Structure2DCapabilities ReosHecRasStructureImporter::capabilities() const
{
    return 0;
}

QString ReosHecRasStructureImporter::crs() const
{
	return QString();
}

QPolygonF ReosHecRasStructureImporter::domain() const
{
	if (!mIsValid || !mController.isValid())
		return QPolygon();

	QStringList areas2d = mController.flowAreas2D();

	if (!areas2d.isEmpty())
		return mController.flow2DAreasDomain(areas2d.first());

}

ReosMeshResolutionController* ReosHecRasStructureImporter::resolutionController(ReosHydraulicStructure2D* structure) const
{
	return new ReosMeshResolutionController(structure,crs());
}
