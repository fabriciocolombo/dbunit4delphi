unit FlatXmlDataSet;

interface

uses DataSet, SysUtils;

type

(*

  - Cada elemento do xml corresponde à uma tupla da tabela. O nome do elemento
    corresponde ao nome da tabela e cada atributo corresponde aos campos da tabela.
  - Para inserir campos nulos, basta omitir os atributos
  - O metadata da tabela será definido baseada na primeira tupla da tabela ou pode
    ser criado um DTD, que será usado como definição do dados.

  O DBUnit possui uma funcionalidade chamada ColumnSensing, que vai adicionando
  as colunas conforme vão sendo usada dentro do xml, por exemplo, caso o primeiro
  registro não possua determinada coluna, ela não fará parte do metadata, mais
  se a coluna existir no segundo registro, o metadata é atualizado. 


  Possível implementação:
    - Criar uma lista onde vão sendo adicionados os DataSets, um para cada tabela.
    - A cada linha, se o dataset não existir, cria o mesmo com a definição das colunas,
      senão, apenas insere o registro no dataset.
    - Após finalizado, ordena a lista de dataset de acordo com a dependência dos dados,
      que pode ser definida de alguma forma
    - Prossegue com a inserção
*)

  TFlatXmlDataSet = class(TInterfacedObject{, IDataSet})
  private
    FFileName: TFileName;
  public
    constructor Create(AFileName: TFileName);
  end;

implementation

{ TFlatXmlDataSet }

constructor TFlatXmlDataSet.Create(AFileName: TFileName);
begin
  FFileName := AFileName;
end;

end.
