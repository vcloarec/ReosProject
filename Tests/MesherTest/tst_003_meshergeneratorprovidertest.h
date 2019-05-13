#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "../../Mesher/provider/meshdataprovider.h"


using namespace testing;

class MesherTesting : public Test
{
public:
    MesherTesting():meshProvider(providerOption)
    {

    }

    TINProvider meshProvider;

    QgsDataProvider::ProviderOptions providerOption;

};








