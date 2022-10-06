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

class ReosHecrasController
{
public:
	ReosHecrasController(const QString& version);
	~ReosHecrasController();

	bool isValid() const;

	QString version() const;


	bool openHecrasProject(const QString& projFileName);

	QStringList flowAreas2D(bool& ok) const;

	static QStringList availableVersion();

	int funcCount = 0;

private:
    class Parameters
    {
    public:
        ~Parameters();

		DISPPARAMS* funcParameters() const;

        void addStringParameter(const QString& string);
		size_t prepareReturnString();
		void freeString(size_t index);
		void prepareReturnLong(LONG& value);

    private:
		mutable DISPPARAMS mParams;
        mutable std::vector<VARIANTARG> args;
		mutable std::vector<DISPID> argsId;
		mutable std::vector<BSTR> mStringParams;

    };

	bool mIsValid = false;
	IDispatch* mDispatch = nullptr;
	QMap<QString, DISPID> mFunctionNames;

	VARIANT invokeFunction(DISPID, const Parameters& params, bool &ok) const;
};


#endif // REOSHECRASCONTROLLER_H
