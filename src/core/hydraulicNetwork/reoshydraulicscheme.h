/***************************************************************************
  reoshydraulicscheme.h - ReosHydraulicScheme

 ---------------------
 begin                : 24.10.2021
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
#ifndef REOSHYDRAULICSCHEME_H
#define REOSHYDRAULICSCHEME_H

/**
 * \brief ReosHydraulicScheme is a clas that represent a scheme of hydraulic simulation.
 * An instance of this class contain:
 * - reference to the meteorological model used for watershed hydrograph calculation
 * - other boundary condition for other type of hydraulic node
 */
class ReosHydraulicScheme
{
  public:
    ReosHydraulicScheme();
};

#endif // REOSHYDRAULICSCHEME_H
