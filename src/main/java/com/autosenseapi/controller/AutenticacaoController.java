package com.autosenseapi.controller;

import com.autosenseapi.controller.swagger.AutenticacaoControllerSwagger;
import com.autosenseapi.jwt.AuthenticationRequest;
import com.autosenseapi.jwt.AuthenticationResponse;
import com.autosenseapi.service.AuthenticationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


import jakarta.validation.Valid;

@RestController
@RequestMapping(path = "/auth", produces = MediaType.APPLICATION_JSON_VALUE)
public class AutenticacaoController implements AutenticacaoControllerSwagger {

	@Autowired
	private AuthenticationService authenticationService;
	
	
	 @PostMapping("/autenticar")
	    public ResponseEntity<AuthenticationResponse> autenticar(@Valid @RequestBody AuthenticationRequest request) {
	        return ResponseEntity.ok(authenticationService.authenticate(request));
	    }
	 
	 
	
	
}
