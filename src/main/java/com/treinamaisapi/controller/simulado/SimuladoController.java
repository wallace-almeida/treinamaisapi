package com.treinamaisapi.controller.simulado;

import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import com.treinamaisapi.controller.swagger.SimuladoControllerSwagger;
import com.treinamaisapi.service.simulado.SimuladoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/simulado", produces = MediaType.APPLICATION_JSON_VALUE)
public class SimuladoController implements SimuladoControllerSwagger {

    private final SimuladoService simuladoService;

    // 1️⃣ Criar simulado
    @PostMapping("/create")
    @Override
    public SimuladoResponse criarSimulado(@RequestBody CriarSimuladoRequest request,
                                          @RequestParam Long usuarioId) {
        return simuladoService.criarSimulado(request, usuarioId);
    }

    // 2️⃣ Listar simulados
    @GetMapping
    @Override
    public List<SimuladoResponse> listarSimulados(@RequestParam Long usuarioId) {
        return simuladoService.listarSimuladosPorUsuario(usuarioId);
    }

    // 3️⃣ Responder simulado
    @PostMapping("/{id}/responder")
    @Override
    public ResultadoSimuladoResponse responderSimulado(@PathVariable Long id,
                                                       @RequestBody RespostaSimuladoRequest request) {
        return simuladoService.responderSimulado(id, request);
    }

    // 4️⃣ Ver resultado
    @GetMapping("/{id}/resultado")
    @Override
    public ResultadoSimuladoResponse verResultado(@PathVariable Long id) {
        return simuladoService.visualizarResultado(id);
    }
}
