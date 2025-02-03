package com.adontoApi.service;

import com.adontoApi.jwt.AuthenticationRequest;
import com.adontoApi.jwt.AuthenticationResponse;
import com.adontoApi.jwt.JwtService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;



@Service
public class AuthenticationService {

    //private final UserRepository userRepository;

    //private final PasswordEncoder passwordEncoder;

	@Autowired
    private JwtService jwtService;

	
    //private final AuthenticationManager authenticationManager;

	/*
	 * public AuthenticationResponse register(RegisterRequest request) { var user =
	 * User.builder() .firstName(request.getFirstName())
	 * .lastName(request.getLastName()) .email(request.getEmail())
	 * .password(passwordEncoder.encode(request.getPassword())) .role(Role.USER)
	 * .build(); userRepository.save(user); var jwtToken =
	 * jwtService.generateToken(user); return AuthenticationResponse.builder()
	 * .token(jwtToken) .build(); }
	 */

    public AuthenticationResponse authenticate(AuthenticationRequest request) {
       // authenticationManager.authenticate(
          //      new UsernamePasswordAuthenticationToken(
             //           request.getUsername(),
                //        request.getPassword()));
        //var user = userRepository.findByEmail(request.getEmail()).orElseThrow();
        var jwtToken = jwtService.generateToken(request.getUsuario());
        return AuthenticationResponse.builder()
                .token(jwtToken)
                .build();
    }

}
