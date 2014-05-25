% generate sounds.

-module(sound).

-export([synthesize/3, sound/4]).

%% set the number of bits uses per sample
%-define(EightBits, true)
-define(SixteenBits, true). %% big endian

%% number of samples per second
-define(SamplingRate, 22050).

% Returns a binary. This is in "raw" or "pcm" format. 
% Settings for sampling rate and sample size are defined above.
% Input: 
%    Frequency (in Hertz)
%    Length (in seconds)
%    Volume (a float between 0 and 1)
%    Shape ('sinus' or 'saw')
sound(Frequency, Length, Volume, Shape) ->
  %% the sound consists of:
  %% an attack (a short crescendo bit), a middle part with stable volume,
  %% and a fadeout bit. 
  %%
  %% The basis is a short synthesized fragment (ca. 5 milliseconds). The parts are created by 
  %% pasting together a number of fragments. The attack gets 2 fragments, the fadeout 4 fragments,
  %% and the middle portion as many as required.
  %%
  %% The fadeout and attack are created by gradually decreasing or increasing the amplitude of 
  %% the wave.
  %% 
  {Fragment, FragmentDuration, Attaque, FadeOut, _SamplesPerFragment, LongFragment} = 
    synthesize(Frequency, Volume, Shape),
  MiddleLength = lists:max([0, Length - (6 * FragmentDuration)]),
  LongFragmentDuration = FragmentDuration * 16,
  NrOfLongFragments = trunc(MiddleLength/LongFragmentDuration),
  MainPart1 = case NrOfLongFragments of
    0 -> 
      FadeOut;
    _ -> 
      lists:foldl(fun(_X, A) -> <<LongFragment/binary, A/binary>> end, FadeOut,
                         lists:seq(1, NrOfLongFragments))
  end,
  NrOfShortFragments = trunc((MiddleLength - NrOfLongFragments * LongFragmentDuration)/FragmentDuration),
  MainPart2 = case NrOfShortFragments of
    0 ->
      MainPart1;
    _ -> 
      lists:foldl(fun(_X, A) -> <<Fragment/binary, A/binary>> end, MainPart1, 
                         lists:seq(1, NrOfShortFragments))
  end,
  <<Attaque/binary, MainPart2/binary>>.


%% returns {Fragment, FragmentDuration, Attaque, FadeOut, SamplesPerFragment, LongFragment}.
synthesize(Frequency, Volume, Shape) ->
  %% the Sound is a number of fragments. the Fragments are synthesized, using a wave formula.
  %% In order to be able to join them into 1 uniform sound, the fragment should consist of a number
  %% of complete waves.
  %% In order to keep the pitch more or less correct, while at the same time 
  %% keeping the number of sinus() calls within limits, the duration of a fragment
  %% is kept more or less stable, which means the number of waves in the sample varies
  %% with the pitch.
  SamplingRate = ?SamplingRate,
  TargetFragmentDuration = case Frequency of
    High when High > 200 -> (1/440 * 4);
    _Low -> (1/440 * 8)
  end,
  WavesPerFragment = round(TargetFragmentDuration * Frequency),
  case Frequency of 
    0 -> io:format("1- Dividing by 0~n");
    _ -> ok
  end,
  Dw = 1/Frequency, % duration of 1 wave
  FragmentDuration = WavesPerFragment * Dw,
  %% sample WaverPerFragment waves - this is fitted into an exact (integer) number of 
  %% samples. This introduces a small error, but I guess this will be 
  %% acceptable.
  TargetSamplesPerWave = Dw * SamplingRate,
  SamplesPerFragment = round(WavesPerFragment*TargetSamplesPerWave),
  case WavesPerFragment of 
    0 -> io:format("2- Dividing by 0, frequency: ~p, Volume: ~p~n", [Frequency, Volume]);
    _ -> ok
  end,
  SamplesPerWave = SamplesPerFragment/WavesPerFragment,
  WaveFunction = 
    case Shape of
      sinus ->
        AnglePerSample = 2 * math:pi()/SamplesPerWave,
        fun(X, Acc) -> sinusWave(X, Volume, AnglePerSample, Acc) end;
      saw ->
        fun(X, Acc) ->
          %% calulate the location in the wave, 0 = extreme left, 100 = extreme right
          L = remainder(X, SamplesPerWave),
          LRelative = L/SamplesPerWave * 100,
          Sample = Volume * 256 * if 
                     LRelative < 10 ->
                       LRelative/10;
                     LRelative < 90 ->
                       (10 - LRelative) * 2/80 + 1;
                     true ->
                       - (100 - LRelative)/10
                   end,
          <<Acc/bitstring, (round(Sample)):2/little-signed-integer-unit:8>>
        end
    end,
  Fragment = lists:foldl(WaveFunction, <<>>, lists:seq(1, SamplesPerFragment)),
  %% The 'attaque' gets 2 fragments. The volume (= amplitude) sound increases 
  %% in a linear way.
  LongFragment = lists:foldl(fun(_X, A) -> <<Fragment/binary, A/binary>> end, Fragment, lists:seq(1, 15)),
  Attaque = crescendo(<<Fragment/binary, Fragment/binary>>, 0, 1),
  %% The 'fade out' gets 4 fragments. The volume (= amplitude) sound decreases 
  %% in a linear way.
  FadeOut = crescendo(<<Fragment/binary, Fragment/binary, Fragment/binary, Fragment/binary>>, 1, 0),
  %% returns {Fragment, FragmentDuration, Attack, Fadeout}
  {Fragment, FragmentDuration, Attaque, FadeOut, SamplesPerFragment, LongFragment}.

remainder(X, Y) -> 
  X - (trunc(X/Y) * Y).

crescendo(Sound, From, To) ->
  NrOfSamples = size(Sound) div 2,
  crescendo(Sound, From, (To - From)/NrOfSamples, <<>>).

crescendo(<<>>, _From, _Step, Acc) ->
  Acc;
crescendo(<<S:2/little-signed-integer-unit:8, Tail/binary>>, Current, Step, Acc) ->
  S2 = round(S * Current),
  crescendo(Tail, Current + Step, Step, <<Acc/binary, S2:2/little-signed-integer-unit:8>>).

-ifdef(EightBits).
sinusWave(X, Amp, AnglePerSample, Acc) ->
  <<Acc/binary, (trunc(math:sin(X * AnglePerSample) * Amp + 128))>>.
-endif.

-ifdef(SixteenBits).
sinusWave(X, Amp, AnglePerSample, Acc) ->
  <<Acc/binary, (trunc(math:sin(X * AnglePerSample) * Amp * 256)):2/little-signed-integer-unit:8>>.
  %% <<Acc/binary, (trunc(math:sin(X * AnglePerSample) * Amp * 256 + 65535/2)):16>>.
-endif.
  
