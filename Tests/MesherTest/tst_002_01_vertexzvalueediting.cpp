#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/ReosMesh/reosmesh.h"
#include "../../Mesher/ReosMesh/reosmeshgenerator.h"


using namespace testing;

class VertexZValueEditingTesting:public Test{
public:

    VertexZSpecifierSimpleFactory simpleZSpecifierFactory;
    VertexZSpecifierOtherVertexAndSlopeFactory slopeZSpecifierFactory;
    VertexZSpecifierOtherVertexAndGapFactory gapZSpecifierFactory;

};


TEST_F(VertexZValueEditingTesting, verticesDistance){

    auto vert1=VertexBasic(0,0);
    auto vert2=VertexBasic(3,4);

    ASSERT_THAT(abs(vert1.distanceFrom(vert2)-5),Lt(std::numeric_limits<double>::min()));
}

TEST_F(VertexZValueEditingTesting, createSimpleSpecifierDefault){

    auto vert1=VertexBasic(0,0);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(vert1.z(),Eq(0));
}

TEST_F(VertexZValueEditingTesting, createSimpleSpecifierWithValue){

    auto vert1=VertexBasic(0,0);
    simpleZSpecifierFactory.setZValue(5);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    ASSERT_THAT(vert1.z(),Eq(5));
}

TEST_F(VertexZValueEditingTesting, createVertexSlopeSpecifier){

    auto vert1=VertexBasic(0,0);
    auto vert2=VertexBasic(5,0);

    simpleZSpecifierFactory.setZValue(5);
    vert1.setZSpecifier(simpleZSpecifierFactory);

    slopeZSpecifierFactory.setSlope(0.05);
    slopeZSpecifierFactory.setOtherVertex(&vert1);
    vert2.setZSpecifier(slopeZSpecifierFactory);

    ASSERT_THAT(abs(vert2.z()-5.25),Lt(std::numeric_limits<double>::min()));
}



TEST_F(VertexZValueEditingTesting, createVertexGapSpecifier){

    auto vert1=VertexBasic(0,0);
    auto vert2=VertexBasic(5,0);
    gapZSpecifierFactory.setGap(0.05);
    gapZSpecifierFactory.setOtherVertex(&vert1);
    vert2.setZSpecifier(gapZSpecifierFactory);

    ASSERT_THAT(abs(vert2.z()-0.05),Lt(std::numeric_limits<double>::min()));
}

