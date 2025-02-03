package com.adontoApi.filter;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@Schema(name = "SupIntegracaoFilter", description = "Filtrar os registros de areasistemica no sigpes")
public class SupIntegracaoFilter {

	
	@Parameter(description = "ID_SUPRIMENTO ,operador equal .", required = false, example = "0917214")
	private Long idSuprimento;
	
	@Parameter(description = "TP_OPERACAO ,operador equal .", required = false, example = "0917214")
	private String tpOperacao;
	
	@Parameter(description = "ST_OPERACAO ,operador equal .", required = false, example = "0917214")
	private String stOperacao;
	
	
	
	

	
}


