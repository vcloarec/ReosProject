/***************************************************************************
  reoshydraulicsimulation.h - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
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
#ifndef REOSHYDRAULICSIMULATION_H
#define REOSHYDRAULICSIMULATION_H

class ReosHydraulicStructure2D;


class ReosHydraulicSimulation
{
  public:
    ReosHydraulicSimulation();

    static void prepareInput( ReosHydraulicStructure2D *hydraulicStructure );

};


#endif // REOSHYDRAULICSIMULATION_H
