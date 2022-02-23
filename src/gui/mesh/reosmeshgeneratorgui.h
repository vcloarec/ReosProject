/***************************************************************************
  reosmeshgeneratorgui.h - ReosMeshGeneratorGui

 ---------------------
 begin                : 13.2.2022
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
#ifndef REOSMESHGENERATORGUI_H
#define REOSMESHGENERATORGUI_H

#include <QList>

class QAction;
class QWidget;
class ReosMeshGenerator;

class ReosMeshGeneratorGui
{
  public:

    static QWidget createResolutionControllerWidget( ReosMeshGenerator *meshGenerator, QWidget *parents );
};



#endif // REOSMESHGENERATORGUI_H
