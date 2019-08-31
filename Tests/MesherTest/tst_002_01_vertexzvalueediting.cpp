#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/ReosMesh/reosmesh.h"
#include "../../Mesher/ReosMesh/reosmeshgenerator.h"


using namespace testing;

class VertexZValueEditingTesting:public Test{
public:

};


TEST_F(VertexZValueEditingTesting, verticesDistance){

    auto vert1=VertexBasic(0,0);
    auto vert2=VertexBasic(3,4);

    ASSERT_THAT(abs(vert1.distanceFrom(vert2)-5),Lt(std::numeric_limits<double>::min()));
}

TEST_F(VertexZValueEditingTesting, createSimpleSpecifierDefault){

    auto vert1=VertexBasic(0,0);
    auto zSpecifier=std::make_unique<VertexZSpecifierSimple>(&vert1);

    ASSERT_THAT(zSpecifier->getZValue(),Eq(0));
}

TEST_F(VertexZValueEditingTesting, createSimpleSpecifierWithValue){

    auto vert1=VertexBasic(0,0);
    auto zSpecifier=std::make_unique<VertexZSpecifierSimple>(&vert1,5);

    ASSERT_THAT(zSpecifier->getZValue(),Eq(5));
}

TEST_F(VertexZValueEditingTesting, createVertexSlopeSpecifier){

    auto vert1=VertexBasic(0,0);
    auto vert2=VertexBasic(5,0);
    auto zSpecifier=std::make_unique<VertexZSpecifierOtherVertexAndSlope>(&vert1,&vert2,-0.05);

    ASSERT_THAT(abs(zSpecifier->getZValue()-0.25),Lt(std::numeric_limits<double>::min()));
}

