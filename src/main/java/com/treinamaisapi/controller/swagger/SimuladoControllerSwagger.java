package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Simulado", description = "Usuarios do sistema")
public interface SimuladoControllerSwagger {


    // 1️⃣ Criar simulado
    @PostMapping("/create")
    SimuladoResponse criarSimulado(@RequestBody CriarSimuladoRequest request,
                                   @RequestParam Long usuarioId);

    // 2️⃣ Listar simulados
    @GetMapping
    List<SimuladoResponse> listarSimulados(@RequestParam Long usuarioId);

    // 3️⃣ Responder simulado
    @PostMapping("/{id}/responder")
    ResultadoSimuladoResponse responderSimulado(@PathVariable Long id,
                                                @RequestBody RespostaSimuladoRequest request);

    // 4️⃣ Ver resultado
    @GetMapping("/{id}/resultado")
    ResultadoSimuladoResponse verResultado(@PathVariable Long id);
}
