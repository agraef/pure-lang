//Set of C++ wave table function 
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: STK-4.3

// converted to C by Dr.Graef@t-online.de, 2011

#ifndef STKLIB_H
#define STKLIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>

//******************************************************
//functions for parameters lookup and interpolation 
//******************************************************

typedef struct {
  // Note: Actual array size is 2*m_nPoints;
  double *m_Points;
  int m_nPoints;
} LookupTable;

static inline LookupTable *newLookupTable(double *points, int num_points)
{
  LookupTable *t = (LookupTable*)malloc(sizeof(LookupTable));
  t->m_Points = points;
  t->m_nPoints = num_points;
  return t;
}

static inline double getValue(LookupTable *t, double x)
{
  // Note: Assumes points are monotonically increasing in X!
	
  int i=0;
  while (x>t->m_Points[i*2] && i<t->m_nPoints)
    i++;
	
  if (i==0)
    return t->m_Points[1];
	
  if (i>=t->m_nPoints)
    return t->m_Points[(t->m_nPoints-1)*2+1];
	
  double ratio =
    (x - t->m_Points[(i-1)*2])
    / (t->m_Points[i*2] - t->m_Points[(i-1)*2]);
	
  return t->m_Points[(i-1)*2+1]*(1-ratio) + t->m_Points[i*2+1]*(ratio);
}

//********************************
//stick for modal synthesis
//********************************

#define TABLE_SIZE 1024

static inline double readMarmstk1(int index){
	static float marmstk1[TABLE_SIZE/4] = {
		0.000579833984375, -0.003417968750000, 0.015930175781250, -0.037689208984375, 0.062866210937500, 
		0.168640136718750, -0.226287841796875, -0.020233154296875, 0.017120361328125, 0.032745361328125, 
		0.028198242187500, -0.065704345703125, 0.102355957031250, -0.135375976562500, -0.088378906250000, 
		0.135375976562500, 0.036987304687500, 0.030181884765625, -0.023498535156250, -0.050872802734375, 
		0.120574951171875, -0.223419189453125, 0.235260009765625, -0.296081542968750, 0.384582519531250, 
		-0.363708496093750, 0.206207275390625, 0.076873779296875, -0.262420654296875, 0.306579589843750, 
		-0.349090576171875, 0.359161376953125, -0.304809570312500, 0.156860351562500, 0.022552490234375, 
		-0.063598632812500, 0.017425537109375, 0.024505615234375, -0.016296386718750, -0.056304931640625, 
		0.093536376953125, -0.108825683593750, 0.215484619140625, -0.354858398437500, 0.316925048828125, 
		-0.164672851562500, 0.028594970703125, 0.095001220703125, -0.165679931640625, 0.218811035156250, 
		-0.239105224609375, 0.182830810546875, -0.026275634765625, -0.016601562500000, -0.042175292968750, 
		0.080566406250000, -0.123352050781250, 0.071563720703125, -0.021514892578125, -0.000488281250000, 
		0.080139160156250, -0.188354492187500, 0.230712890625000, -0.172271728515625, 0.033325195312500, 
		0.111236572265625, -0.127532958984375, 0.118682861328125, -0.136383056640625, 0.068878173828125, 
		0.041931152343750, -0.126129150390625, 0.134155273437500, -0.024902343750000, -0.094726562500000, 
		0.136840820312500, -0.140930175781250, 0.123962402343750, -0.080383300781250, -0.033691406250000, 
		0.167541503906250, -0.194976806640625, 0.151489257812500, -0.042388916015625, -0.028625488281250, 
		0.030853271484375, -0.079559326171875, 0.071166992187500, 0.026977539062500, -0.075714111328125, 
		0.110107421875000, -0.076507568359375, -0.043426513671875, 0.063110351562500, -0.099487304687500, 
		0.137664794921875, -0.086181640625000, 0.047119140625000, 0.022491455078125, -0.092956542968750, 
		0.070709228515625, -0.036560058593750, -0.004943847656250, 0.051208496093750, -0.042541503906250, 
		0.042114257812500, -0.024414062500000, -0.039916992187500, 0.082580566406250, -0.094451904296875, 
		0.039459228515625, 0.037048339843750, -0.061218261718750, 0.080810546875000, -0.070159912109375, 
		0.037139892578125, 0.008789062500000, -0.078094482421875, 0.094024658203125, -0.048431396484375, 
		0.009643554687500, 0.020263671875000, -0.032379150390625, 0.021820068359375, -0.021270751953125, 
		-0.033203125000000, 0.102172851562500, -0.089721679687500, 0.052856445312500, -0.001495361328125, 
		-0.070404052734375, 0.109436035156250, -0.104156494140625, 0.116302490234375, -0.074310302734375, 
		-0.004425048828125, 0.061309814453125, -0.090698242187500, 0.056732177734375, -0.015380859375000, 
		-0.010406494140625, 0.019622802734375, 0.000213623046875, -0.017272949218750, 0.065399169921875, 
		-0.119842529296875, 0.105499267578125, -0.051391601562500, -0.024383544921875, 0.085968017578125, 
		-0.099731445312500, 0.121948242187500, -0.098876953125000, 0.038085937500000, 0.034362792968750, 
		-0.071441650390625, 0.039550781250000, -0.017272949218750, -0.001708984375000, 0.031402587890625, 
		-0.027740478515625, 0.013183593750000, 0.013488769531250, -0.083831787109375, 0.103637695312500, 
		-0.061645507812500, 0.026947021484375, 0.036499023437500, -0.078735351562500, 0.089294433593750, 
		-0.090393066406250, 0.034820556640625, 0.019500732421875, -0.070129394531250, 0.102569580078125, 
		-0.070922851562500, 0.039672851562500, 0.020507812500000, -0.078674316406250, 0.065002441406250, 
		-0.045806884765625, 0.027801513671875, 0.012115478515625, -0.018829345703125, 0.015594482421875, 
		-0.010772705078125, -0.042938232421875, 0.062103271484375, -0.032745361328125, 0.004791259765625, 
		0.028137207031250, -0.067687988281250, 0.078094482421875, -0.063049316406250, 0.039215087890625, 
		0.012359619140625, -0.052337646484375, 0.074401855468750, -0.063629150390625, 0.034362792968750, 
		0.013732910156250, -0.044189453125000, 0.042419433593750, -0.047210693359375, 0.019897460937500, 
		0.020538330078125, -0.039825439453125, 0.048675537109375, -0.025726318359375, -0.016998291015625, 
		0.038482666015625, -0.056060791015625, 0.061584472656250, -0.014343261718750, -0.023101806640625, 
		0.051849365234375, -0.069854736328125, 0.043853759765625, -0.016662597656250, 0.002380371093750, 
		0.033721923828125, -0.039733886718750, 0.021148681640625, -0.010375976562500, 0.000000000000000, 
		0.000000000000000, 0.000000000000000, -0.000030517578125, 0.000030517578125, 0.000000000000000, 
		0.000000000000000, -0.000030517578125, -0.000030517578125, 0.000030517578125, 0.000030517578125, 
		-0.000061035156250, 0.000000000000000, 0.000000000000000, 0.000000000000000, 0.000030517578125, 
		-0.000030517578125, 0.000000000000000, 0.000030517578125, -0.000030517578125, 0.000000000000000, 
		0.000061035156250, -0.000061035156250, 0.000030517578125, 0.000000000000000, -0.000030517578125, 
		0.000000000000000, 0.000061035156250, 0.000000000000000, -0.000030517578125, 0.000000000000000, 
		0.000030517578125};
	return marmstk1[index];
};

//Parameters for bass.dsp
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: FAUST-STK

static double bassLoopFilterb0_points[19*2] = {
	24.000,0.54355,
	26.000,0.54355,
	27.000,0.55677,
	29.000,0.55677,
	32.000,0.55677,
	33.000,0.83598,
	36.000,0.83598,
	43.000,0.83598,
	44.000,0.88292,
	48.000,0.88292,
	51.000,0.88292,
	52.000,0.77805,
	54.000,0.77805,
	57.000,0.77805,
	58.000,0.91820,
	60.000,0.91820,
	61.000,0.91820,
	63.000,0.94594,
	65.000,0.91820,
};

static LookupTable *bassLoopFilterb0;

static inline float getValueBassLoopFilterb0(float index){
  if (!bassLoopFilterb0)
    bassLoopFilterb0 = newLookupTable(&bassLoopFilterb0_points[0], 18);
  return getValue(bassLoopFilterb0, index);
}

static double bassLoopFilterb1_points[19*2] = {
	24.000,-0.36586,
	26.000,-0.36586,
	27.000,-0.37628,
	29.000,-0.37628,
	32.000,-0.37628,
	33.000,-0.60228,
	36.000,-0.60228,
	43.000,-0.60228,
	44.000,-0.65721,
	48.000,-0.65721,
	51.000,-0.65721,
	52.000,-0.51902,
	54.000,-0.51902,
	57.000,-0.51902,
	58.000,-0.80765,
	60.000,-0.80765,
	61.000,-0.80765,
	63.000,-0.83230,
	65.000,-0.83230,
};

static LookupTable *bassLoopFilterb1;

static inline float getValueBassLoopFilterb1(float index){
  if (!bassLoopFilterb1)
    bassLoopFilterb1 = newLookupTable(&bassLoopFilterb1_points[0], 18);
  return getValue(bassLoopFilterb1, index);
}

static double bassLoopFiltera1_points[19*2] = {
	24.000,-0.81486,
	26.000,-0.81486,
	27.000,-0.81147,
	29.000,-0.81147,
	32.000,-0.81147,
	33.000,-0.76078,
	36.000,-0.76078,
	43.000,-0.76078,
	44.000,-0.77075,
	48.000,-0.77075,
	51.000,-0.77075,
	52.000,-0.73548,
	54.000,-0.73548,
	57.000,-0.73548,
	58.000,-0.88810,
	60.000,-0.88810,
	61.000,-0.88810,
	63.000,-0.88537,
	65.000,-0.88537,
};

static LookupTable *bassLoopFiltera1;

static inline float getValueBassLoopFiltera1(float index){
  if (!bassLoopFiltera1)
    bassLoopFiltera1 = newLookupTable(&bassLoopFiltera1_points[0], 18);
  return getValue(bassLoopFiltera1, index);
}

//Parameters for harpsichord.dsp
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: FAUST-STK

static double DryTapAmpT60_points[16*2] = {
//  NoteNumber(A440=69), T60(sec)
	21.001,0.491,
	26.587,0.498,
	34.249,0.470,
	40.794,0.441,
	47.977,0.392,
	55.000,0.370,
	60.000,0.370,
	66.000,0.370,
	71.934,0.370,
	78.000,0.370,
	83.936,0.390,
	88.557,0.387, 
	92.858,0.400,
	97.319,0.469,
	102.400,0.500,
	107.198,0.494	};

static LookupTable *DryTapAmpT60;

static inline float getValueDryTapAmpT60 (float index){
  if (!DryTapAmpT60)
    DryTapAmpT60 = newLookupTable(&DryTapAmpT60_points[0], 16);
  return getValue(DryTapAmpT60, index);
}

static double releaseLoopGain_points[10*2] = {
	21.000,0.865,
	24.000,0.880,
	29.000,0.896,
	36.000,0.910,
	48.000,0.920,
	60.000,0.950,
	72.000,0.965,
	84.000,0.988,
	88.000,0.997,
	99.000,0.988	};

static LookupTable *releaseLoopGain;

static inline float getValueReleaseLoopGain(float index){
  if (!releaseLoopGain)
    releaseLoopGain = newLookupTable(&releaseLoopGain_points[0], 10);
  return getValue(releaseLoopGain, index);
}

static double loopFilterb0_points[18*2] = {
	35.000,0.94373,
	36.000,0.94731,
	46.000,0.94731,
	47.000,0.96202,
	52.000,0.96202,
	53.000,0.97477,
	58.000,0.97477,
	59.000,0.97733,
	64.000,0.97733,
	65.000,0.97971,
	70.000,0.97971,
	71.000,0.97971,
	76.000,0.97971,
	77.000,0.98698,
	82.000,0.98698,
	83.000,0.98462,
	86.000,0.98462,
	87.000,0.98611,
};

static LookupTable *loopFilterb0;

static inline float getValueLoopFilterb0(float index){
  if (!loopFilterb0)
    loopFilterb0 = newLookupTable(&loopFilterb0_points[0], 18);
  return getValue(loopFilterb0, index);
}

static double loopFilterb1_points[18*2] = {
	35.000,0.60010,
	36.000,-0.59124,
	46.000,-0.59124,
	47.000,-0.21243,
	52.000,-0.21243,
	53.000,-0.39280,
	58.000,-0.39280,
	59.000,-0.48307,
	64.000,-0.48307,
	65.000,0.51965,
	70.000,0.51965,
	71.000,0.51965,
	76.000,0.51965,
	77.000,-0.42463,
	82.000,-0.42463,
	83.000,0.85655,
	86.000,0.85655,
	87.000,0.68851,
};

static LookupTable *loopFilterb1;

static inline float getValueLoopFilterb1(float index){
  if (!loopFilterb1)
    loopFilterb1 = newLookupTable(&loopFilterb1_points[0], 18);
  return getValue(loopFilterb1, index);
}

static double loopFilterb2_points[18*2] = {
	35.000,-0.00360,
	36.000,-0.12249,
	46.000,-0.12249,
	47.000,-0.16044,
	52.000,-0.16044,
	53.000,-0.21680,
	58.000,-0.21680,
	59.000,-0.16346,
	64.000,-0.16346,
	65.000,0.22162,
	70.000,0.22162,
	71.000,0.22162,
	76.000,0.22162,
	77.000,-0.14973,
	82.000,-0.14973,
	83.000,0.24937,
	86.000,0.24937,
	87.000,0.14838,
};

static LookupTable *loopFilterb2;

static inline float getValueLoopFilterb2(float index){
  if (!loopFilterb2)
    loopFilterb2 = newLookupTable(&loopFilterb2_points[0], 18);
  return getValue(loopFilterb2, index);
}

static double loopFiltera1_points[18*2] = {
	35.000,0.5941,
	36.000,-0.65928,
	46.000,-0.65928,
	47.000,-0.24222,
	52.000,-0.24222,
	53.000,-0.41402,
	58.000,-0.41402,
	59.000,-0.50837,
	64.000,-0.50837,
	65.000,0.51263,
	70.000,0.51263,
	71.000,0.51263,
	76.000,0.51263,
	77.000,-0.43976,
	82.000,-0.43976,
	83.000,0.85396,
	86.000,0.85396,
	87.000,0.68332,
};

static LookupTable *loopFiltera1;

static inline float getValueLoopFiltera1(float index){
  if (!loopFiltera1)
    loopFiltera1 = newLookupTable(&loopFiltera1_points[0], 18);
  return getValue(loopFiltera1, index);
}

static double loopFiltera2_points[18*2] = {
	35.000,-0.02641,
	36.000,-0.10275,
	46.000,-0.10275,
	47.000,-0.15842,
	52.000,-0.15842,
	53.000,-0.21653,
	58.000,-0.21653,
	59.000,-0.15833,
	64.000,-0.15833,
	65.000,0.22025,
	70.000,0.22025,
	71.000,0.22025,
	76.000,0.22025,
	77.000,-0.14583,
	82.000,-0.14583,
	83.000,0.24405,
	86.000,0.24405,
	87.000,0.14370,
};

static LookupTable *loopFiltera2;

static inline float getValueLoopFiltera2(float index){
  if (!loopFiltera2)
    loopFiltera2 = newLookupTable(&loopFiltera2_points[0], 18);
  return getValue(loopFiltera2, index);
}

//Parameters for piano.dsp
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: FAUST-STK

/* pianoDriverC.sb */
/* Coupling Filter */

static LookupTable *singleStringDecayRate;
static LookupTable *singleStringZero;
static LookupTable *singleStringPole;
static LookupTable *detuningHz;
static LookupTable *stiffnessCoefficient;
static LookupTable *strikePosition;
static LookupTable *EQGain;
static LookupTable *EQBandwidthFactor;

/* PianoDriverA */
/* HammerFilter */
static LookupTable *loudPole;
static LookupTable *softPole;
static LookupTable *loudGain;
static LookupTable *softGain;


/* Soundboard */
static LookupTable *sustainPedalLevel;
static LookupTable *DCBa1;


/* pianoDriverB */
/* High Notes */
static LookupTable *secondStageAmpRatio;
static LookupTable *r1_1db;
static LookupTable *r1_2db;
static LookupTable *r2db;
static LookupTable *r3db;
static LookupTable *secondPartialFactor;
static LookupTable *thirdPartialFactor;
static LookupTable *bq4_gEarBalled;

//***************************************************************

/* pianoDriverC.sb */
/* Coupling Filter */

double singleStringDecayRate_points[17*2] = {
	21.000,-1.500,
	24.000,-1.500,
	28.000,-1.500,
	29.000,-6.000,
	36.000,-6.000,
	42.000,-6.100,
	48.000,-7.000,
	52.836,-7.000,
	60.000,-7.300,
	66.000,-7.700,
	72.000,-8.000,
	78.000,-8.800,
	84.000,-10.000,
	88.619,-11.215,
	92.368,-12.348,
	95.684,-13.934,
	99.000,-15.000
};

double singleStringZero_points[17*2] = {
	21.000,-1.000,
	24.000,-1.000,
	28.000,-1.000,
	29.000,-1.000,
	32.534,-1.000,
	36.000,-0.700,
	42.000,-0.400,
	48.000,-0.200,
	54.000,-0.120,
	60.000,-0.080,
	66.000,-0.070,
	72.000,-0.070,
	79.000,-0.065,
	84.000,-0.063,
	88.000,-0.060,
	96.000,-0.050,
	99.000,-0.050	};

double singleStringPole_points[17*2] = {
	21.000,0.350,
	24.604,0.318,
	26.335,0.279,
	28.000,0.250,
	32.000,0.150,
	36.000,0.000,
	42.000,0.000,
	48.000,0.000,
	54.000,0.000,
	60.000,0.000,
	66.000,0.000,
	72.000,0.000,
	76.000,0.000,
	84.000,0.000,
	88.000,0.000,
	96.000,0.000,
	99.000,0.000	};

double detuningHz_points[18*2] = {
	21.000,0.003,
	24.000,0.003,
	28.000,0.003,
	29.000,0.060,
	31.000,0.100,
	36.000,0.110,
	42.000,0.120,
	48.000,0.200,
	54.000,0.200,
	60.000,0.250,
	66.000,0.270,
	72.232,0.300,
	78.000,0.350,
	84.000,0.500,
	88.531,0.582,
	92.116,0.664,
	95.844,0.793,
	99.000,1.000	};

double stiffnessCoefficient_points[10*2] = {
	21.000,-0.850,
	23.595,-0.850,
	27.055,-0.830,
	29.000,-0.700,
	37.725,-0.516,
	46.952,-0.352,
	60.000,-0.250,
	73.625,-0.036,
	93.810,-0.006,
	99.000,1.011	};

double strikePosition_points[12*2] = {
	21.000,0.050,
	24.000,0.050,
	28.000,0.050,
	35.000,0.050,
	41.000,0.050,
	42.000,0.125,
	48.000,0.125,
	60.000,0.125,
	72.000,0.125,
	84.000,0.125,
	96.000,0.125,
	99.000,0.125	};

double EQGain_points[14*2] = {
	21.000,2.000,
	24.000,2.000,
	28.000,2.000,
	30.000,2.000,
	35.562,1.882,
	41.000,1.200,
	42.000,0.600,
	48.000,0.500,
	54.000,0.500,
	59.928,0.502,
	66.704,0.489,
	74.201,0.477,
	91.791,1.000,
	99.000,1.000	};

double EQBandwidthFactor_points[13*2] = {
	21.000,5.000,
	24.112,5.000,
	28.000,5.000,
	35.000,4.956,
	41.000,6.000,
	42.000,2.000,
	48.773,1.072,
	57.558,1.001,
	63.226,1.048,
	69.178,1.120,
	72.862,1.525,
	80.404,2.788,
	97.659,1.739	};


/* PianoDriverA */
/* HammerFilter */

double loudPole_points[19*2] = {
	21.000,0.875,
	23.719,0.871,
	27.237,0.836,
	28.996,0.828,
	32.355,0.820,
	36.672,0.816,
	40.671,0.820,
	45.788,0.812,
	47.867,0.812,
	54.000,0.810,
	60.000,0.800,
	66.000,0.800,
	72.000,0.810,
	78.839,0.824,
	84.446,0.844,
	89.894,0.844,
	96.463,0.848,
	103.512,0.840,
	107.678,0.840	};

double softPole_points[16*2] = {
	21.000,0.990,
	24.000,0.990,
	28.000,0.990,
	29.000,0.990,
	36.000,0.990,
	42.000,0.990,
	48.000,0.985,
	54.000,0.970,
	60.000,0.960,
	66.000,0.960,
	72.000,0.960,
	78.000,0.970,
	84.673,0.975,
	91.157,0.990,
	100.982,0.970,
	104.205,0.950	};

double loudGain_points[16*2] = {
	21.873,0.891,
	25.194,0.870,
	30.538,0.848,
	35.448,0.853,
	41.513,0.842,
	47.434,0.826,
	53.644,0.820,
	60.720,0.815,
	65.630,0.820,
	72.995,0.853,
	79.060,0.920,
	85.270,1.028,
	91.624,1.247,
	95.668,1.296,
	99.000,1.300,
	100.000,1.100	};

double softGain_points[15*2] = {
	20.865,0.400,
	22.705,0.400,
	25.960,0.400,
	28.224,0.400,
	31.196,0.400,
	36.715,0.400,
	44.499,0.400,
	53.981,0.400,
	60.000,0.350,
	66.000,0.350,
	72.661,0.350,
	81.435,0.430,
	88.311,0.450,
	93.040,0.500,
	96.434,0.500	};


/* Soundboard */

double sustainPedalLevel_points[13*2] = {
	21.000,0.050,
	24.000,0.050,
	31.000,0.030,
	36.000,0.025,
	48.000,0.010,
	60.000,0.005,
	66.000,0.003,
	72.000,0.002,
	78.000,0.002,
	84.000,0.003,
	90.000,0.003,
	96.000,0.003,
	108.000,0.002	};

double DCBa1_points[18*2] = {
	21.000,-0.999,
	24.000,-0.999,
	30.000,-0.999,
	36.000,-0.999,
	42.000,-0.999,
	48.027,-0.993,
	60.000,-0.995,
	72.335,-0.960,
	78.412,-0.924,
	84.329,-0.850,
	87.688,-0.770,
	91.000,-0.700,
	92.000,-0.910,
	96.783,-0.850,
	99.000,-0.800,
	100.000,-0.850,
	104.634,-0.700,
	107.518,-0.500	};

/* pianoDriverB */
/* High Notes */

double secondStageAmpRatio_points[6*2] = {
	82.277,-18.508,
	88.000,-30.000,
	90.000,-30.000,
	93.451,-30.488,
	98.891,-30.633,
	107.573,-30.633	};

double r1_1db_points[3*2] = {
	100.000,-75.000,
	103.802,-237.513,
	108.000,-400.000	};

double r1_2db_points[4*2] = {
	98.388,-16.562,
	100.743,-75.531,
	103.242,-154.156,
	108.000,-300.000	};

double r2db_points[2*2] = {
	100.000,-115.898,
	107.858,-250.000	};

double r3db_points[2*2] = {
	100.000,-150.000,
	108.000,-400.000	};

double secondPartialFactor_points[2*2] = {
	88.000,2.000,
	108.000,2.100	};

double thirdPartialFactor_points[2*2] = {
	88.000,3.100,
	108.000,3.100	};

double bq4_gEarBalled_points[6*2] = {
	100.000,0.040,
	102.477,0.100,
	104.518,0.300,
	106.000,0.500,
	107.000,1.000,
	108.000,1.500	};

//************************************************************************

static inline float getValueSustainPedalLevel (float index){
  if (!sustainPedalLevel) sustainPedalLevel = newLookupTable(&sustainPedalLevel_points[0], 13);
  return getValue(sustainPedalLevel, index);
}

static inline float getValueLoudPole(float index){
  if (!loudPole) loudPole = newLookupTable(&loudPole_points[0], 19);
  return getValue(loudPole, index);
}

static inline float getValuePoleValue(float index){
  if (!softPole) softPole = newLookupTable(&softPole_points[0], 16);
  return getValue(softPole, index);
}

static inline float getValueLoudGain(float index){
  if (!loudGain) loudGain = newLookupTable(&loudGain_points[0], 16);
  return getValue(loudGain, index);
}

static inline float getValueSoftGain(float index){
  if (!softGain) softGain = newLookupTable(&softGain_points[0], 15);
  return getValue(softGain, index);
}

static inline float getValueDCBa1(float index){
  if (!DCBa1) DCBa1 = newLookupTable(&DCBa1_points[0], 18);
  return getValue(DCBa1, index);
}

static inline float getValuer1_1db(float index){
  if (!r1_1db) r1_1db = newLookupTable(&r1_1db_points[0], 3);
  return getValue(r1_1db, index);
}

static inline float getValuer1_2db(float index){
  if (!r1_2db) r1_2db = newLookupTable(&r1_2db_points[0], 4);
  return getValue(r1_2db, index);
}

static inline float getValuer2db(float index){
  if (!r2db) r2db = newLookupTable(&r2db_points[0], 2);
  return getValue(r2db, index);
}

static inline float getValuer3db(float index){
  if (!r3db) r3db = newLookupTable(&r3db_points[0], 2);
  return getValue(r3db, index);
}

static inline float getValueSecondStageAmpRatio(float index){
  if (!secondStageAmpRatio) secondStageAmpRatio = newLookupTable(&secondStageAmpRatio_points[0], 6);
  return getValue(secondStageAmpRatio, index);
}

static inline float getValueSecondPartialFactor(float index){
  if (!secondPartialFactor) secondPartialFactor = newLookupTable(&secondPartialFactor_points[0], 2);
  return getValue(secondPartialFactor, index);
}

static inline float getValueThirdPartialFactor(float index){
  if (!thirdPartialFactor) thirdPartialFactor = newLookupTable(&thirdPartialFactor_points[0], 2);
  return getValue(thirdPartialFactor, index);
}

static inline float getValueBq4_gEarBalled(float index){
  if (!bq4_gEarBalled) bq4_gEarBalled = newLookupTable(&bq4_gEarBalled_points[0], 6);
  return getValue(bq4_gEarBalled, index);
}

static inline float getValueStrikePosition(float index){
  if (!strikePosition) strikePosition = newLookupTable(&strikePosition_points[0], 12);
  return getValue(strikePosition, index);
}

static inline float getValueEQBandWidthFactor(float index){
  if (!EQBandwidthFactor) EQBandwidthFactor = newLookupTable(&EQBandwidthFactor_points[0], 13);
  return getValue(EQBandwidthFactor, index);
}

static inline float getValueEQGain(float index){
  if (!EQGain) EQGain = newLookupTable(&EQGain_points[0], 14);
  return getValue(EQGain, index);
}

static inline float getValueDetuningHz(float index){
  if (!detuningHz) detuningHz = newLookupTable(&detuningHz_points[0], 18);
  return getValue(detuningHz, index);
}

static inline float getValueSingleStringDecayRate(float index){
  if (!singleStringDecayRate) singleStringDecayRate = newLookupTable(&singleStringDecayRate_points[0], 17);
  return getValue(singleStringDecayRate, index);
}

static inline float getValueSingleStringZero(float index){
  if (!singleStringZero) singleStringZero = newLookupTable(&singleStringZero_points[0], 17);
  return getValue(singleStringZero, index);
}

static inline float getValueSingleStringPole(float index){
  if (!singleStringPole) singleStringPole = newLookupTable(&singleStringPole_points[0], 17);
  return getValue(singleStringPole, index);
}

static inline float getValueStiffnessCoefficient(float index){
  if (!stiffnessCoefficient) stiffnessCoefficient = newLookupTable(&stiffnessCoefficient_points[0], 10);
  return getValue(stiffnessCoefficient, index);
}

//Modal datas for modalBar.dsp
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: STK-4.3

static inline float loadPreset(int preset, int index0, int index1){
	static float presets[9][4][4] = { 
		{{1.0, 3.99, 10.65, -2443},		// Marimba
			{0.9996, 0.9994, 0.9994, 0.999},
			{0.04, 0.01, 0.01, 0.008},
			{0.429688, 0.445312, 0.093750}},
		{{1.0, 2.01, 3.9, 14.37}, 		// Vibraphone
			{0.99995, 0.99991, 0.99992, 0.9999},	
			{0.025, 0.015, 0.015, 0.015 },
			{0.390625,0.570312,0.078125}},
		{{1.0, 4.08, 6.669, -3725.0},		// Agogo 
			{0.999, 0.999, 0.999, 0.999},	
			{0.06, 0.05, 0.03, 0.02},
			{0.609375,0.359375,0.140625}},
		{{1.0, 2.777, 7.378, 15.377},		// Wood1
			{0.996, 0.994, 0.994, 0.99},	
			{0.04, 0.01, 0.01, 0.008},
			{0.460938,0.375000,0.046875}},
		{{1.0, 2.777, 7.378, 15.377},		// Reso
			{0.99996, 0.99994, 0.99994, 0.9999},	
			{0.02, 0.005, 0.005, 0.004},
			{0.453125,0.250000,0.101562}},
		{{1.0, 1.777, 2.378, 3.377},		// Wood2
			{0.996, 0.994, 0.994, 0.99},	
			{0.04, 0.01, 0.01, 0.008},
			{0.312500,0.445312,0.109375}},
		{{1.0, 1.004, 1.013, 2.377},		// Beats
			{0.9999, 0.9999, 0.9999, 0.999},	
			{0.02, 0.005, 0.005, 0.004},
			{0.398438,0.296875,0.070312}},
		{{1.0, 4.0, -1320.0, -3960.0},		// 2Fix
			{0.9996, 0.999, 0.9994, 0.999},	
			{0.04, 0.01, 0.01, 0.008},
			{0.453125,0.453125,0.070312}},
		{{1.0, 1.217, 1.475, 1.729},		// Clump
			{0.999, 0.999, 0.999, 0.999},	
			{0.03, 0.03, 0.03, 0.03 },
			{0.390625,0.570312,0.078125}},
	};
	return presets[preset][index0][index1];
};

//Phonemes datas for voiceForm.dsp
//©Romain Michon (rmichon@ccrma.stanford.edu), 2011
//licence: STK-4.3

static inline float loadPhonemeGains(int index0, int index1) {
	static float phonemeGains[32][2] =
	{{1.0, 0.0},    // eee
		{1.0, 0.0},    // ihh
		{1.0, 0.0},    // ehh
		{1.0, 0.0},    // aaa

		{1.0, 0.0},    // ahh
		{1.0, 0.0},    // aww
		{1.0, 0.0},    // ohh
		{1.0, 0.0},    // uhh

		{1.0, 0.0},    // uuu
		{1.0, 0.0},    // ooo
		{1.0, 0.0},    // rrr
		{1.0, 0.0},    // lll

		{1.0, 0.0},    // mmm
		{1.0, 0.0},    // nnn
		{1.0, 0.0},    // nng
		{1.0, 0.0},    // ngg

		{0.0, 0.7},    // fff
		{0.0, 0.7},    // sss
		{0.0, 0.7},    // thh
		{0.0, 0.7},    // shh

		{0.0, 0.7},    // xxx
		{0.0, 0.1},    // hee
		{0.0, 0.1},    // hoo
		{0.0, 0.1},    // hah

		{1.0, 0.1},    // bbb
		{1.0, 0.1},    // ddd
		{1.0, 0.1},    // jjj
		{1.0, 0.1},    // ggg

		{1.0, 1.0},    // vvv
		{1.0, 1.0},    // zzz
		{1.0, 1.0},    // thz
		{1.0, 1.0}     // zhh
  };
	return phonemeGains[index0][index1];
}
	
static inline float loadPhonemeParameters(int index0, int index1, int index2){
	static float phonemeParameters[32][4][3] =
  {{  { 273, 0.996,  10},       // eee (beet)
      {2086, 0.945, -16}, 
      {2754, 0.979, -12}, 
      {3270, 0.440, -17}},
   {  { 385, 0.987,  10},       // ihh (bit)
      {2056, 0.930, -20},
      {2587, 0.890, -20}, 
      {3150, 0.400, -20}},
   {  { 515, 0.977,  10},       // ehh (bet)
      {1805, 0.810, -10}, 
      {2526, 0.875, -10}, 
      {3103, 0.400, -13}},
   {  { 773, 0.950,  10},       // aaa (bat)
      {1676, 0.830,  -6},
      {2380, 0.880, -20}, 
      {3027, 0.600, -20}},
     
   {  { 770, 0.950,   0},       // ahh (father)
      {1153, 0.970,  -9},
      {2450, 0.780, -29},
      {3140, 0.800, -39}},
   {  { 637, 0.910,   0},       // aww (bought)
      { 895, 0.900,  -3},
      {2556, 0.950, -17},
      {3070, 0.910, -20}},
   {  { 637, 0.910,   0},       // ohh (bone)  NOTE::  same as aww (bought)
      { 895, 0.900,  -3},
      {2556, 0.950, -17},
      {3070, 0.910, -20}},
   {  { 561, 0.965,   0},       // uhh (but)
      {1084, 0.930, -10}, 
      {2541, 0.930, -15}, 
      {3345, 0.900, -20}},
    
   {  { 515, 0.976,   0},       // uuu (foot)
      {1031, 0.950,  -3},
      {2572, 0.960, -11},
      {3345, 0.960, -20}},
   {  { 349, 0.986, -10},       // ooo (boot)
      { 918, 0.940, -20},
      {2350, 0.960, -27},
      {2731, 0.950, -33}},
   {  { 394, 0.959, -10},       // rrr (bird)
      {1297, 0.780, -16},
      {1441, 0.980, -16},
      {2754, 0.950, -40}},
   {  { 462, 0.990,  +5},       // lll (lull)
      {1200, 0.640, -10},
      {2500, 0.200, -20},
      {3000, 0.100, -30}},
     
   {  { 265, 0.987, -10},       // mmm (mom)
      {1176, 0.940, -22},
      {2352, 0.970, -20},
      {3277, 0.940, -31}},
   {  { 204, 0.980, -10},       // nnn (nun)
      {1570, 0.940, -15},
      {2481, 0.980, -12},
      {3133, 0.800, -30}},
   {  { 204, 0.980, -10},       // nng (sang)    NOTE:: same as nnn
      {1570, 0.940, -15},
      {2481, 0.980, -12},
      {3133, 0.800, -30}},
   {  { 204, 0.980, -10},       // ngg (bong)    NOTE:: same as nnn
      {1570, 0.940, -15},
      {2481, 0.980, -12},
      {3133, 0.800, -30}},
     
   {  {1000, 0.300,   0},       // fff
      {2800, 0.860, -10},
      {7425, 0.740,   0},
      {8140, 0.860,   0}},
   {  {0,    0.000,   0},       // sss
      {2000, 0.700, -15},
      {5257, 0.750,  -3}, 
      {7171, 0.840,   0}},
   {  { 100, 0.900,   0},       // thh
      {4000, 0.500, -20},
      {5500, 0.500, -15},
      {8000, 0.400, -20}},
   {  {2693, 0.940,   0},       // shh
      {4000, 0.720, -10},
      {6123, 0.870, -10},
      {7755, 0.750, -18}},

   {  {1000, 0.300, -10},       // xxx           NOTE:: Not Really Done Yet
      {2800, 0.860, -10},
      {7425, 0.740,   0},
      {8140, 0.860,   0}},
   {  { 273, 0.996, -40},       // hee (beet)    (noisy eee)
      {2086, 0.945, -16}, 
      {2754, 0.979, -12}, 
      {3270, 0.440, -17}},
   {  { 349, 0.986, -40},       // hoo (boot)    (noisy ooo)
      { 918, 0.940, -10},
      {2350, 0.960, -17},
      {2731, 0.950, -23}},
   {  { 770, 0.950, -40},       // hah (father)  (noisy ahh)
      {1153, 0.970,  -3},
      {2450, 0.780, -20},
      {3140, 0.800, -32}},
     
   {  {2000, 0.700, -20},       // bbb           NOTE:: Not Really Done Yet
      {5257, 0.750, -15},
      {7171, 0.840,  -3}, 
      {9000, 0.900,   0}},
   {  { 100, 0.900,   0},       // ddd           NOTE:: Not Really Done Yet
      {4000, 0.500, -20},
      {5500, 0.500, -15},
      {8000, 0.400, -20}},
   {  {2693, 0.940,   0},       // jjj           NOTE:: Not Really Done Yet
      {4000, 0.720, -10},
      {6123, 0.870, -10},
      {7755, 0.750, -18}},
   {  {2693, 0.940,   0},       // ggg           NOTE:: Not Really Done Yet
      {4000, 0.720, -10},
      {6123, 0.870, -10},
      {7755, 0.750, -18}},
     
   {  {2000, 0.700, -20},       // vvv           NOTE:: Not Really Done Yet
      {5257, 0.750, -15},
      {7171, 0.840,  -3}, 
      {9000, 0.900,   0}},
   {  { 100, 0.900,   0},       // zzz           NOTE:: Not Really Done Yet
      {4000, 0.500, -20},
      {5500, 0.500, -15},
      {8000, 0.400, -20}},
   {  {2693, 0.940,   0},       // thz           NOTE:: Not Really Done Yet
      {4000, 0.720, -10},
      {6123, 0.870, -10},
      {7755, 0.750, -18}},
   {  {2693, 0.940,   0},       // zhh           NOTE:: Not Really Done Yet
      {4000, 0.720, -10},
      {6123, 0.870, -10},
      {7755, 0.750, -18}}
  };
	return phonemeParameters[index0][index1][index2];
}

#ifdef __cplusplus
}
#endif

#endif // ! STKLIB_H
