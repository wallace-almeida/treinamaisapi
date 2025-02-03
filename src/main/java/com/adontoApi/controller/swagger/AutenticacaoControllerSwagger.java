package com.adontoApi.controller.swagger;

import com.adontoApi.jwt.AuthenticationRequest;
import com.adontoApi.jwt.AuthenticationResponse;
import org.springframework.http.ResponseEntity;


import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Autenticacao", description = "Autenticacao")
public interface AutenticacaoControllerSwagger {

	
	@Operation(summary = "Autenticacao", description = "Autenticacao")
	public ResponseEntity<AuthenticationResponse> autenticar(AuthenticationRequest request);
	
	

}
