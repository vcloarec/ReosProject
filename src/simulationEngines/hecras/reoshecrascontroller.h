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

class IDispatch;

class ReosHecrasController
{
public:
	ReosHecrasController();
	~ReosHecrasController();

	bool isValid() const;

private:
	bool mIsValid = false;
	IDispatch* mDispatch = nullptr;
};


#endif // REOSHECRASCONTROLLER_H
