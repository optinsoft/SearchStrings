# Search Strings

## Description

Class TSearchTree implements a trie (also called "digital tree" or "prefix tree", see: https://en.wikipedia.org/wiki/Trie).

In SearchStrings folder you can find sample application that allows to compare performance of trie search and linear (sequential) search.

## Requirements

Delphi 10.2 Tokyo or newer.

## Usage

```pascal
uses uSearchStrings, uIntList;

var
  LSearcher: TSearchTree;
  LSearchList: TStringList;
  LFoundList: TIntList;
  LFoundAt, LFoundIndex: Integer;
begin
  LSearcher := TSearchTree.Create; 
  LSearchList := TStringList.Create;
  LFoundList := TIntList.Create;
  try
    LSearchList.Add('john');
    LSearchList.Add('mary');
    LSearcher.MatchType := smtContains;
    LSearcher.CaseSensitive := False;
    LSearcher.Build(LSearchList);
    LFoundAt := LSearcher.FindMatch('Doe John', LFoundIndex);
    if LFoundAt > 0 then begin
      //LFoundAt contains 5 - position (>= 1) of the found string from LSearchList ('john')
      //LFoundIndex contains 0 - index of the found string ('john')
    end;
    if LSearcher.FindAllMatches('John loves Mary', LFoundList) then begin
      //LFoundList contains [0, 1] - indexes of found  strings ('john' and 'mary')
    end;
  finally
    LFoundList.Free;
    LSearchList.Free;
    LSearcher.Free;
  end;
end;
```

## License

BSD 2-Clause License.

## Contributions

Search Strings Sample project uses TReadOnlyCachedFileStream and TWriteCachedFileStream by David Heffernan (https://stackoverflow.com/questions/5639531/buffered-files-for-faster-disk-access)
