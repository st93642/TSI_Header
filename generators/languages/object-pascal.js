/**
 * Object Pascal/Delphi Code Base Generator
 * Generates boilerplate Object Pascal/Delphi code
 */

function generateObjectPascalCodeBase() {
    return `\n{ Basic Object Pascal/Delphi program }\n\nprogram HelloWorld;\n\n{\$APPTYPE CONSOLE}\n\nuses\n  SysUtils;\n\ntype\n  { Example class definition }\n  TTSIStudent = class\n  private\n    FName: string;\n    FProgram: string;\n  public\n    constructor Create(const AName, AProgram: string);\n    procedure Introduce;\n    property Name: string read FName;\n    property Program: string read FProgram;\n  end;\n\n{ TTSIStudent implementation }\nconstructor TTSIStudent.Create(const AName, AProgram: string);\nbegin\n  FName := AName;\n  FProgram := AProgram;\nend;\n\nprocedure TTSIStudent.Introduce;\nbegin\n  Writeln('Hello, I''m ', FName, ' from TSI!');\nend;\n\n{ Main program }\nvar\n  Student: TTSIStudent;\nbegin\n  try\n    Writeln('Hello, World!');\n    Writeln('This is a basic Object Pascal program.');\n    \n    { Example usage }\n    Student := TTSIStudent.Create('TSI Student', 'Computer Science');\n    try\n      Student.Introduce;\n    finally\n      Student.Free;\n    end;\n    \n  except\n    on E: Exception do\n      Writeln(E.ClassName, ': ', E.Message);\n  end;\nend.\n`;
}

module.exports = {
    generateObjectPascalCodeBase
};