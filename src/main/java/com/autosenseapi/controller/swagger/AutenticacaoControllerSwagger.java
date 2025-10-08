package com.autosenseapi.controller.swagger;

import com.autosenseapi.jwt.AuthenticationRequest;
import com.autosenseapi.jwt.AuthenticationResponse;
import org.springframework.http.ResponseEntity;


import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Autenticacao", description = "Autenticacao")
public interface AutenticacaoControllerSwagger {

	
	@Operation(summary = "Autenticacao", description = "Autenticacao")
	public ResponseEntity<AuthenticationResponse> autenticar(AuthenticationRequest request);
	
	

}
